#lang scheme
; A variable with state
(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount)) ; the (set! balance (- balance amount)) expression decrements the balance
             balance)
    "Insufficient funds"))

; Redefining withdraw so that balance is an internal variable
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; A withdraw procedure with "withdrawal processors"
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
(W2 70)
(W2 40)
(W1 40)



; A "bank-account object" with an initial balance specified as input
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

; Use of make-account
(define acc (make-account 100))
((acc 'withdraw) 50)
((acc 'withdraw) 60)
((acc 'deposit) 40)
((acc 'withdraw) 60)

; Completely new object with a separate balance
(define acc2 (make-account 100))



; Random integer procedure
(define random-init 4)  ; To avoid errors
(define (rand-update x) (+ x 1))  ; Just here to avoid errors, this isn't what this procedure would look like
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))



; Monte Carlo procedure fir estimating pi
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
    (iter trials 0))

; Same procedure, but using rand-update directly instead of rand
(define (estimate-pi-rand-update trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 random-init))



; make-withdraw, but without checking for insufficient amount
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))
(W 20)
(W 10)

; Making a decrementer without set!
(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))
(define D (make-decrementer 25))
(D 20)
(D 10)



; Decrementer example - same behavior of subtracting input from 25
(define D1 (make-decrementer 25))
(define D2 (make-decrementer 25))
(D1 10)
(D2 10)

; make-simplified-withdraw example - "same" behavior, but with respect to different internal variables
(define W3 (make-simplified-withdraw 25))
(define W4 (make-simplified-withdraw 25))
(W3 20)
(W3 20)
(W4 20)



; Iterative factorial program from chapter 1
(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

; Imperative version of iterative factorial
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))