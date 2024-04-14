#lang scheme

#| Procedures |#
(define (square x)
  (* x x))


(define (sum-of-squares x y)
  (+ (square x) (square y)))


(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))



#| special form `cond` |#
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))



#| Procedure for finding a good square root estimate |#
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  
(define (improve guess x)
    (average guess (/ x guess)))


(define (average x y)
    (/ (+ x y) 2))


(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))



#| The square root procedure, but simplified using lexical scoping |#
(define (lexically-scoped-sqrt x)
  (define (good-enough? guess)
    (< (abs ( - (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

  