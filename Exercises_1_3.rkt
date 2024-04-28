#lang scheme
; Exercise 1.29
(define (simpsons-rule f a b n)
  (define h (/ (- b a) (* n 1.0)))
  (define (simpson-term k)
    (define arg (+ a (* k h)))
    (cond ((or (= 0 k) (= n k)) (f arg))
          ((= (remainder k 2) 1) (* 4 (f arg)))
          (else (* 2 (f arg)))))
  (* (/ h 3)
     (sum simpson-term 0 inc n)))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n)
  (+ n 1))

(define (cube x) (* x x x))
(simpsons-rule cube 0 1 100)
(simpsons-rule cube 0 1 1000)
; These results are much more accurate than the normal integration function used in the code follow along.
; Note - Given that Dr. Racket's precision seems to truncate at 15 decimal places, it sometimes looks like larger values of n give
;    less accurate results. This isn't the case mathematically, and is just an illusion of the the way computers handle floats.
;    After a certain point, the extra precision gained by more iterations is too small to be displayed.



; Exercise 1.30
(define (iter-sum term a next b)
  (define (iter a result)
    (if (> (a b))
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))



; Exercise 1.31
; Product procedure
(define (product f a b)
  (if (> a b)
      1
      (* (f a) (product f (+ a 1) b))))
(define (identity x)
  x)

; Factorial procedure
(define (factorial x) ; Assumes x is a non-negative integer
  (if (< x 1)
      1
      (product identity 1 x)))

; Product procedure for estimating pi
(define (pi-estimate-term i)
  (cond ((= 1 i) (/ 2.0 3.0))
        ((= (remainder i 2) 1)
         (/ (+ i 1) (+ i 2)))
        (else (/ (+ i 2) (+ i 1)))))
(product pi-estimate-term 1 10000)

; Iterative product process
(define (iter-product f n result)
  (if (= 0 n)
      result
      (iter-product f (- n 1) (* result (f n)))))



; Exercise 1.32
; General accumulation procedure (recursive)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

; Creation of sum procedure using accumulation
(define (accum-sum term a next b)
  (accumulate + 0 term a next b))

; Creation of product procedure using accumulation
(define (accum-product term a next b)
  (accumulate * 1 term a next b))

; General accumulation procedure (iterative)
(define (accumulate-iter combiner f a b result) ; Result acts as the null-value here
  (if (> a b)
      result
      (accumulate-iter combiner (+ a 1) b (combiner result (f a)))))



; Exercise 1.33
; Filtered accumulate procedure
(define (filtered-accumulate combiner f a b result filter)
  (cond ((> a b) result)
        ((filter a)
         (filtered-accumulate combiner f (+ a 1) b (combiner result (f a)) filter))
        (else (filtered-accumulate combiner f (+ a 1) b result filter))))

; Sum of squares of prime numbers in interval a to b.
(define (square x) (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))
(filtered-accumulate + square 1 10 0 prime?)

; Product of all positive integers less than n that are relatively prime to n
; Create the gcd filter
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (gcd-filter a b)
  (if (= (gcd a b) 1)
      true
      false))
; Since the filter for this function has to do with the current value in the range's *relation to n*, we need a new accumulation function whose filter takes two arguments
(define (filtered-accumulate-two combiner f a b result filter)
  (cond ((> a b) result)
        ((filter a b)
         (filtered-accumulate-two combiner f (+ a 1) b (combiner result (f a)) filter))
        (else (filtered-accumulate-two combiner f (+ a 1) b result filter))))
; Solution
(filtered-accumulate-two * identity 1 10 1 gcd-filter)



; Exercise 1.34
#|
(define (f g) (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))
|#
; (f f)
; -> (f 2)
; -> (2 2)
; This will throw an error, because the second f's argument will be an integer (2) instead of a procedure



; Exercise 1.35 - Showing that the golden ratio is a fixed point of the given function is in Liquid Text
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)



; Exercise 1.36
(define (displayed-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess) (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; Fixed point of f(x) = log(1000)/log(x)
(define (y-func x)
  (/ (log 1000) (log x)))

; Without damping, 34 steps
(displayed-fixed-point y-func 2.0)

; WIth damping, 9 steps
(displayed-fixed-point (lambda (x) (/ (+ (y-func x) x) 2)) 2.0)



; Exercise 1.37
; Recursive cont-frac
(define (cont-frac n d k)
  (define (recursive-call i)
    (if (= k i)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recursive-call (+ i 1))))))
  (recursive-call 1))

; Takes 10 iterations
(< (abs (- 0.618033988749894848204586
        (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)))
   0.0001)

; Iterative cont-frac
(define (cont-frac-iter n d k result) ; Starting result here should be (d k)
  (if (= 1 k)
      (/ (n k) result)
      (cont-frac-iter n
                      d
                      (- k 1)
                      (+ (d (- k 1)) (/ (n k) result)))))



; Exercise 1.38
(define (int-div a b)
  (- (/ a b) (/ (remainder a b) b)))
(define (d-proc i)
  (if (= (remainder (+ i 1) 3) 0)
      (* (+ (int-div i 3) 1) 2)
      1))
(+ (cont-frac (lambda (i) 1.0) d-proc 100) 2) ; Estimating e



; Exercise 1.39
(define (tan-cf x k)
  (define (cont-frac-tan d)
    (define (recursive-call i)
    (if (= k i)
        (/ (if (= 1 k)
               x
               (* (square x) -1))
           (d i))
        (/ (if (= 1 i)
               x
               (* (square x) -1))
           (+ (d i) (recursive-call (+ i 1))))))
  (recursive-call 1))
  (cont-frac-tan d-tan))
(define (d-tan i)
 (- (* i 2) 1))

; Estimate tan(pi/4) = 1
(tan-cf (/ 3.14159 4) 100)



; Exercise 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Solution
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))



; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))
((double inc) 4)

(((double (double double)) inc) 5) ; Returns 21, simplification notes in LiquidText



; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))
((compose square inc) 6)



; Exercise 1.43
(define (repeated f k)
  (define (recurse start)
    (if (= start (- k 1))
        (lambda (x) (f x))
        (compose f (recurse (+ start 1)))))
  (recurse 0))
((repeated square 2) 5)



; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ ((f (- x dx)) (f x) (f (+ x dx)))) 3)))

; Repeated smoothing
(define (repeated-smoothing f n)
  ((repeated smooth n) f))



; Exercise 1.45
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (average x y)
    (/ (+ x y) 2))
(define (power n x)
  (if (= n 1)
      x
      (* x (power (- n 1) x))))

; Nth root
(define (nth-root n x damp-count)
  (fixed-point ((repeated average-damp damp-count) (lambda (y) (/ x (power (- n 1) y))))
               1.0))

; Procedure to calculate roots until one of them hangs to find pattern
(define (calculate-roots n x damp-count)
  (newline)
  (display n)
  (display " -> ")
  (display (nth-root n x damp-count))
  (calculate-roots (+ n 1) 1000 damp-count))

; (calculate-roots 2 1000 1) One damp count fails on 4th roots
; (calculate-roots 2 1000 2) Two damps fails on 8th roots
; (calculate-roots 2 1000 3) Three damps fails on 16th roots
; (calculate-roots 2 1000 4) Four damps fails on 32 roots
; (calculate-roots 2 1000 5) Five damps fails on 64 roots
; (calculate-roots 2 1000 6) Six damps fails on 128 roots
; (calculate-roots 2 1000 7) Seven damps fails on 256 roots
; Conclusion is that k damps will be able to calculate up to (but not including) the 2^(k + 1)th root

; Solution: Nth-root function where damp-count dynamically calculated based on the root requested
(define (nth-root-augmented n x)
    (fixed-point ((repeated average-damp (damp-count n))
                  (lambda (y) (/ x (power (- n 1) y))))
               1.0))
(define (damp-count nth-root)
  (define (damp-finder start)
    (if (< nth-root (power (+ start 1) 2))
        start
        (damp-finder (+ start 1))))
  (damp-finder 1))

; Test nth-root-augmented procedure
(define (calculate-roots-augmented n x)
  (newline)
  (display n)
  (display " -> ")
  (display (nth-root-augmented n x))
  (calculate-roots-augmented (+ n 1) x))
        
  
  

