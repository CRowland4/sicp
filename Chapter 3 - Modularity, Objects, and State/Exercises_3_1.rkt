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



; Exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch user-password m)
    (cond ((not (eq? user-password password))
           (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)