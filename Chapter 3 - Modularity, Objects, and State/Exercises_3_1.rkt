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
     (+ low (* (random) range)))) 

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


  
  