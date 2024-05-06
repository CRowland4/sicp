#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (make-product (caddr p) (cadddr p))))
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (make-sum (caddr s) (cdddr s))))
(define (multiplier p)
  (cadr p))
(define (addend s)
  (cadr s))
(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)  ; One of the factors is 0
        ((=number? m1 1) m2)  ; One of the factors is 1
        ((=number? m2 1) m1)  ; One of the factors is 1
        ((and (number? m1) (number? m2)) (* m1 m2))  ; Both of the factors are numbers that aren't 0 or 1
        (else (list '* m1 m2))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)  ; One of the items is 0
        ((=number? a2 0) a1)  ; One of the items is 0
        ((and (number? a1) (number? a2))  ; Both items are numbers
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? expr num)
  (and (number? expr) (= expr num)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (non-numbers default items)
  (let ((accumulation (accumulate cons '() (filter (lambda (x) (not (number? x))) items))))
    (if (null? accumulation)
        default
        accumulation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.53
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes bue socks)



; Exercise 2.54
(define (equal? list1 list2)
  (cond ((or (and (not (pair? list1)) (pair? list2))
             (and (not (pair? list2)) (pair? list1)))

         false)
        ((and (not (pair? list1)) (not (pair? list2)) (eq? list1 list2))
         true)
        (else (and (equal? (car list1) (car list2))
                   (equal? (cdr list1) (cdr list2))))))
(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f



; Exercise 2.55
(car ''abracadabra)
#|
The innter >'abracadabra< is the same as (quote abracadabra).
Then you have '(quote abracadabra), which is a list of symbols, the first of which is 'quote,
   and (car <a list of symbols starting with 'quote>) is going to return 'quote, which is displayed as > quote.
|#



; Exercise 2.56
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var)))
        ((and (exponentiation? expr) (number? (exponent expr)))
         (make-product (exponent expr)
                       (make-product (make-exponentiation (base expr) (- (exponent expr) 1))
                                     (deriv (base expr) var))))
        ((product? expr)
         (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        (else
         (error "unknown expression type: DERIV" exp))))
(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define (exponent expr)
  (caddr expr))
(define (base expr)
  (cadr expr))
(define (make-exponentiation base expn)
  (cond ((and (number? base) (number? expn))
         (expt base expn))
        ((=number? expn 1) base)
        ((=number? expn 0) 1)
        (else (list '** base expn))))



; Exercise 2.57
(define (multiplicand-new p)
  (if (= (length p) 3)
      (caddr p)
      (make-product (caddr p) (cadddr p))))
(define (augend-new s)
  (if (= (length s) 3)
      (caddr s)
      (make-sum (caddr s) (cdddr s))))


