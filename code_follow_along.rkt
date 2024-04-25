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



; Recursive procedure for Fibonacci numbers
(define (recursive-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (recursive-fib (- n 1))
                 (recursive-fib (- n 2))))))

; Iterative procedure for Fibonacci numbers
(define (iterative-fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))



; Making change with n coins
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))



; Recursive exponentiation procedure
(define (recursive-expt b n)
  (if (= n 0)
      1
      (* b (recursive-expt b (- n 1)))))

; Iterative exponentiaion procedure
(define (iterative-expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; Faster iterative exponentiation procedure
(define (expt b n)
  (fast-expt-iter b n 1))
(define (fast-expt-iter b counter product)
  (if (= counter 0)
      product
      (fast-expt-iter b
                 (- counter 1)
                 (* b product))))



; Euclid's algorithm
(define (gcd a b)
  (if (= b -)
      a
      (gcd b (remainder a b))))



; Algorithm for finding the smallest integer divisor greater than 1 of a given number n
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

; Use the smallest divisor algorithm to test if a number is prime
(define (prime? n)
  (= n (smallest-divisor n)))



; Algorithm to find the (exponential of a number) modulo another number 
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

; Fermat Test for prime numbers
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; Run the Fermat Test a given number of times to check for primality
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))