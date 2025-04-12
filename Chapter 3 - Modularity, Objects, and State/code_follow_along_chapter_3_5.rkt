#lang sicp
; Everything between here and the row of semi-colons is defined for access, and isn't in the order in which it's presented in the book

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
      1
      0))

(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

(define (parallel-execute)
  "FOO")

(define (make-serializer-temp)
  "BAR")

(define (make-mutex-temp)
  "BAZ")

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (square x)
  (* x x))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (stream-map-generalized proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-generalized
              (cons proc (map stream-cdr argstreams))))))

(define (average x y)
  (/ (+ x y) 2))

(define (partial-sums s)
  (add-streams (cons-stream 0 (partial-sums s)) s))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 3.5 - Streams 
; Standard iterative program for computing the sum of all primes in an interval
(define (sum-primes-iter a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
           (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

; Sum of primes using sequence operations from chapter 2
(define (sum-primes-fast a b)
  (accumulate +
              0
              (filter prime?
                      (enumerate-interval a b))))


; Stream analogues of list procedures from chapter 2
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval
                  10000 1000000))))



; Memoized procedure
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))



; Stream of positive integers - example of an infinite stream
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers-from-one (integers-starting-from 1))

; Stream of integers not divisible by 7
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers-from-one))

; Stream of Fibonacci numbers
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))



; Sieve of Eratosthenes
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))



; Infinite stream of ones
(define ones (cons-stream 1 ones))

; Element-wise sum of two given streams
(define (add-streams s1 s2) (stream-map-generalized + s1 s2))

; Alternate definition of integers
(define integers
  (cons-stream 1 (add-streams ones integers)))

; Alternate definition of fibs
(define fibs-alternate
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs-alternate) fibs-alternate))))



; Scaling a stream
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))
(define double (cons-stream 1 (scale-stream double 2)))



; Alternate primes
(define primes-alternate
  (cons-stream
   2
   (stream-filter alternate-prime? (integers-starting-from 3))))
(define (alternate-prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes-alternate))



; Old sqrt improvement function used previously in an iterator
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

; A stream of guesses for sqrt, rather than storing iterative guesses in a local state variable
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)



; Stream of better and better guesses to pi
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))



; Euler transform of a sequence (It's absolutely insane/amazing that this works)
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

; A tableau, a recursive series accelerator
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

; Using the tableau
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))



; Stream of all pairs of integers (i, j) with i <= j such that i + j is prime
;   It's assumed that we have the sequence <int-pairs>, which is the sequence
;   of all pairs of integers (i, j) with i <= j.
#|
(stream-filter
  (lambda (pair) (prime? (+ (car pair) (cadr pair))))
|#

; Stream of int pairs
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave  ; Defined below
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))






