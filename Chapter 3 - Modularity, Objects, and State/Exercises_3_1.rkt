#lang scheme
; Exercise 3.1
(define (make-accumulator value)
    (lambda (amount)
      (begin (set! value (+ value amount))
             value)))