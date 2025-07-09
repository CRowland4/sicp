#lang sicp
; Everything between here and the row of semi-colons is defined so the exercises have access to them
(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)  
        ((string? exp) true)
        (else false)))

(define (variable? exp)
  (symbol? exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 4.1
(define (left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first-left (eval (first-operand exps) env)))
        (let ((rest (list-of-values (rest-operands exps) env)))
          (cons first-left rest)))))

(define (right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (let ((first-left (eval (first-operand exps) env)))
          (cons first-left rest)))))

#|
For starters, I WAY overcomplicated this at first. Gotta stop overthinking.

Also thank you Barry Allison yet again from wizardbook.wordpress.com for pointing out
  the very subtle detaile that nested lets are required here, since a single let is just shorthand
  for a lambda procedure with 2 expressions, and the expressions in that lambda procedure will be
  evaluated in the default order of the underlying implementation, which doesn't actually fix the
  ambiguity here.
|#



; Exercise 4.2
; Louis's evaluator;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))

        (else
         (error "Unknown expression type: EVAL" exp))))

; a)
#|
In Louis's <eval>, the <self-evaluating?>, <variable?>, and <quoted?> clauses will be missed,
  but since <application?> just checks if the expression is a pair, it will be triggered.
  so we will get (apply (eval (operator exp) env) (list-of-values (operands exp) env)).

(eval (operator exp) env) will give us (eval define env), which will trigger <variable?>, which will then
  throw an error because there is no variable bound to the symbol 'define.
|#

; b)
; The following changes will have to be made. Reason for change is given above the change
; Since this change is for applications, we have to change the <application?> procedure:
(define (application? exp)
  (tagged-list? exp 'call))  ; Instead of (pair? exp)

; <apply> is then called, with (eval (operator exp) env) first, so we'll have to change how the operator is taken from an application
(define (operator exp)
  (cadr exp))  ; Instead of (car exp)

; Similarly, since <apply>'s second argument is (list-of-values (operands exp) env), we'll have to modify <operands>
(define (operands exp)
  (cddr exp))  ; Instead of (cdr exp)



; Exercise 4.3
#|
For a data-directed dispatch procedure, we assume that every non-self-evaluating, non-variable expression has its data type as its car.
  We check for an application next (it made sense to me to have this separate from the lookup table), and then use a data-directed dispatch
  to continue the loop. I also made the choice to strip off the type tag when passing in the argument to the retrieved procedure, it also
  makes more sense to me.

I'm waving my hands over the procedures that would construct the actual table and put/get the procedures from it.
|#
(define (eval-dispatch exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((application? exp)
         (apply (eval-dispatch (operator exp) env)
                (list-of-values (operands exp) env)))
        ((get-syntax (type-tag exp))
         ((get-syntax (type-tag exp)) (cdr exp) env))
        (else
         (error "Unknown expression type: EVAL" exp))))
         
#|
My rough solution here differs the most from the derivative procedure of Exercise 2.73 because of the last thing mentioned above - in that
  procedure, the 'type-tag' was a procedure itself (the operator), and the handling of the operator was done by the procedure retrieved from
  the table, rather than by the dispatcher itself.
|#



; Exercise 4.4
#|
I'm choosing the 'Alternatively,...' option here and implementing 'and' and 'or' as derived expressions.
|#
(and (= x 1) (= y 1) (= z 1) (= a 1))


(cond ((= x 1) 'true)
      ((= y 1) 'true)
      ((= z 1) 'true)
      ((= a 1) 'true)
      (else
       'false))

; and
; We would add these two pieces to eval, taking advatage of the fact that we already have cond->if:
((and? exp) (eval (and->cond exp) env))
((or? exp) (eval (or->cond exp) env))

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









































