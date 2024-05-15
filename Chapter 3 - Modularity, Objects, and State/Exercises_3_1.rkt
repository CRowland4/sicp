#lang scheme
; Exercise 3.1
(define (make-accumulator value)
    (lambda (amount)
      (begin (set! value (+ value amount))
             value)))



; Exercise 3.2
(define (make-monitored f)
  (let ((counter 0))
    (define (mf x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count)
             (begin (set! counter 0)
                    "Counter reset to 0"))
            (else
             (begin (set! counter (+ counter 1))
                    (f x)))))
    mf))