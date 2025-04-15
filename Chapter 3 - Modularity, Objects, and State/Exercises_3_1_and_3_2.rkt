#lang sicp
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



; Exercise 3.4
(define (make-account-secure balance password)
  (define incorrect-passwords 0)

  (define (call-the-cops x)
    "Cops have been called!")
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
           (begin
             (set! incorrect-passwords (+ incorrect-passwords 1))
             (if (> incorrect-passwords 7)
                 call-the-cops
                 (lambda (x) "Incorrect password"))))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)



; Exercise 3.5
(define (square x) (* x x))
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

 (define (random-in-range low high) 
   (let ((range (- high low))) 
     (+ low (* (random range)))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (in-area)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (let ((rectangle-area (* (- x2 x1) (- y2 y1))))
   (* (monte-carlo trials in-area) rectangle-area 1.0)))

(define (unit-circle-predicate x y)
  (<= (+ (square (- x 2)) (square (- y 2))) 1))

(define (pi-estimate)
  (estimate-integral unit-circle-predicate 0.0 4.0 0.0 4.0 5000000))



; Exercise 3.6
(define (rand-update x) (+ x 1))  ; Just here to avoid errors, this isn't what this procedure would look like
(define random-init 5)
(define rand
  (let ((x random-init))
    (lambda (symbol)
      (cond ((eq? symbol 'generate)
             (set! x (rand-update x))
             x)
            ((eq? symbol 'reset)
             (lambda (new-value)
               (set! x new-value)
               (display "Internal random number generator state reset to ")
               (display new-value)))
            (else (error "Symbol not recognized RAND:" symbol))))))



; Exercise 3.7
(define (make-joint password-protected-account account-password new-password)
  (begin ((password-protected-account account-password 'add-password) new-password)
         password-protected-account))

(define (make-account-modified balance . password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (add-password new-password)
    (begin (set! password (cons new-password password))
           "Password added"))
  (define (dispatch user-password m)
    (cond ((not (memq user-password password))
           (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'add-password) add-password)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)



; Exercise 3.8
(define f
  (let ((call-count 0))
    (lambda (x)
      (begin (set! call-count (+ call-count 1))
             (if (= (remainder call-count 2) 1)
                 1
                 (- x))))))



; Exercise 3.9 - in LiquidText



; Exercise 3.10 - in LiquidText



; Exercise 3.11 - in LiquidText

