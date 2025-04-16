#lang sicp
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