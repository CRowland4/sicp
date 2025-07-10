#lang sicp
#|
NOTE - my solution to 4.4 could potentially mishandle falsy values, look out for that (I don't immediately see why but Claude warnted of that.
My assumption was that the predicates of the cond clause would evaluate to booleans, but obviously in a production environment truthy/falsy would
  have to be considered as well.
|#


#| This is the follow-along for Chapter 4, but I'm also including my solutions to problems in their relevant places, because I want to try and actually get the full interpreter up
     and running eventually.
|#


; 4.4 SOLUTION - adding the derived procedures 'and' and 'or' to the evaluator
; The procedures for and->cond and or->cond
(define (and->cond exp)
  (make-cond (negate-all (conjunction-predicates exp)) (conjunction-predicates exp) 'true))
(define (or->cond exp)
  (make-cond (conjunction-predicates exp) (conjunction-predicates exp) 'false))

; The helper procedures
(define (make-cond predicates consequences else-exp)
  (if (not (= (length predicates) (length consequences)))
      (error "Predicates and consequences must be of equal length: MAKE-COND" predicates consequences)
      (if (null? predicates)
          (error "Must have at least one predicate and one consequence: MAKE-COND" predicates consequences)
          (cons 'cond (make-cond-body predicates consequences else-exp)))))
(define (negate-all exps)
  (if (null? exps)
      '()
      (cons (list 'not (car exps)) (negate-all (cdr exps)))))
(define (make-cond-body predicates consequences else-exp)
  (if (null? predicates)
      (list (list 'else else-exp))
      (cons (list (car predicates) (car consequences))
            (make-cond-body (cdr predicates) (cdr consequences) else-exp))))
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items))))) 
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (conjunction-predicates exp) (cdr exp))



; 4.5 SOLUTION - Adding compatibility with the cond arrow syntax
; To modify cond the evaluation of cond to handle the arrow syntax, we need to add to the expand-clauses procedure
(define (arrow-clause? clause)
  (eq? '=> (cadr clause)))
(define (arrow-clause-test clause)
  (car clause))
(define (arrow-clause-recipient clause)
  (caddr clause))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The follow along

; Definition of <eval>
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)  ; If the expression is self evaluating, just return the expression
        ((variable? exp) (lookup-variable-value exp env))  ; If the expression is a variable, lookup the value of that variable in the given environment <env>
        ((and? exp) (eval (and->cond exp) env))  ; SOLUTION TO 4.4 a)
        ((or? exp) (eval (or->cond exp) env))  ; SOLUTION TO 4.4 b)
        ((quoted? exp) (text-of-quotation exp))  ; If the expression is quoted, just return the expression's text, without the quote
        ((assignment? exp) (eval-assignment exp env))  ; If the expression is an assignment, we have to modify the environment to modify the variable binding, then call eval again
        ((definition? exp) (eval-definition exp env))  ; Same as above, except here we're creating a new variable, rather than reassigning an existing one
        ((if? exp) (eval-if exp env))  ; Evaluate the correct part of the expression depending on the value of the predicate (true or false)
        ((lambda? exp) (make-procedure (lambda-parameters exp)  ; If the expression is a lambda, we have to make a procedure out of the parameters and body of the lambda, and package it with the environment <env>
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))  ; For a begin expression, we have to evaluate the sequence of expressions in the order they appear
        ((cond? exp) (eval (cond->if exp) env))  ; For a cond expression, we turn the predicates and expressions into nested if statements, and then evaluate
        ((application? exp)  ; If the expression is apply, we have to apply the value of the operator of the expression to the values of the operands of the expression
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

; Definition of <apply>
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)  ; If the procedure we get is a primitive procedure, we just apply it directly, no further evaluation needed
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)  ; For a compund procedure, we take each expression in the sequence of the body of the procedure, create a new environment that extends from the procedure's original environment
         (eval-sequence                   ;    and contains the bindings of the procedure's formal parameters to the argumnets it was called with, and evaluate each of those expressions with it's corresponding environment
          (procedure-body procedure)      ;    using eval.
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

; Definition of <list-of-values>, used in <eval> for an 'apply' expression
(define (list-of-values exps env)  ; These <exps> are the operands from whatever is being 'applied' in <eval>
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))  ; We recursively return a list of those operands, as the name of the procedure suggests

; Definition of <eval-if>
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)  ; If the predicate of the expression is true, we want to evaluate the consequent
      (eval (if-alternative exp) env)))  ; Otherwise, we evaluate the alternative

; Definition of <eval-sequence>
(define (eval-sequence exps env)  ; Evaluates the expressions in <exps> in the order in which they occur, and returns the value of the final expression
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; Definition of <eval-assignment>
(define (eval-assignment exp env)  ; Assigns the variable <(assignment-variable exp)> to the given value <(eval (assignment-value exp) env)> in the given environment <env>
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)  ; 'ok' is the symbol returned as the value from either a variable assignment or a variable definition (below)

; Definition of <eval-definition>
(define (eval-definition exp env)  ; Similar to <eval-assignment> except now we are creating (defining) a new variable rather than assigning an existing one
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                     env)
  'ok)



; Here we start the procedures that define the syntax specification for our new 'language'
; Only numbers and strings will be self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)  
        ((string? exp) true)
        (else false)))

; Variables are just represented by symbols
(define (variable? exp)
  (symbol? exp))

; This procedure will come in handy for classifying certain expressions
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; Quotations are of the form (quote <text-of-quotation>) - the evaluator sees a quoted expression as a list that starts with the word <quote>.
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp)
  (cadr exp))

; Assignments are of the form (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp)
  (caddr exp))

; Defining a variable or procedure always starts with "(define ....)"
(define (definition? exp)  
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))  ; Variable definitions have two forms
      (cadr exp)  ; (define <var> <value>), for defining simple variables, and (define <var> (lambda (<paramater 1> ... <parameter n>) <body>)) for the under-the-hood version of procedure definition
      (caadr exp)))  ;  For the standard procedure definition of (define (<var> <parameter 1> <parameter 2> ... <parameter n>) <body>)
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)  ; For variable definition, and lambda procedure definitions
      (make-lambda (cdadr exp)  ; Formal parameters of a standard procedure definition
                   (cddr exp))))  ; The body of a standard procedure definition

  ; Making a lambda expression
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Lambda expressions are always lists that start with <lambda>, with the form (lambda (<paramater 1> <parameter 2> ... <parameter n>) <body>)
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

; Conditionals are structured (if <predicate> <consequent> <alternative>), where the alternative is optional
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))  ; If the conditional doesn't have it's own alternative, we supply <false>

; Constructor to make if statements, to be used when evaluating a cond expression
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Begin expressions are of the form (begin (exp 1) (exp 2) ... (exp n))
(define (begin? exp)
  (tagged-list? 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))

; Transforming a sequence into a single expression, with a make-begin procedure if needed
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

; A procedure application is anything that isn't something above, and will be a pair - (<operator> . <list of operands>)
(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))

; The evaluation of a cond expression
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false  ; This is if the cond has no else clause, which Scheme doesn't specify, but we're choosing as false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ; Added by me for Exercise 4.5
            (if (arrow-clause? first)
                (make-if (arrow-clause-test first)
                         (list arrow-clause-recipient (arrow-clause-test first))
                         (expand-clauses rest))
                ;;;;;;;;;;;;;;;;;;;;;;;;;;;
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))




  
        

