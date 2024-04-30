#lang scheme
; Procedures defined between here and the separator are here so that the exercises can access them
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 2.1
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
         (if (or (and (< n 0) (< d 0))
          (and (> n 0) (> d 0)))
      (cons (abs (/ n g)) (abs (/ d g)))
      (cons (- (abs (/ n g))) (abs (/ d g))))))
(make-rat 2 4)
(make-rat -2 -4)
(make-rat -2 4)
(make-rat 2 -4)
