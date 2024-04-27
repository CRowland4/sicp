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
  (if (= b 0)
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



; Procedure for cubing
(define (cube x)
  (* x x x))



; Procedure for sum of integers from a to b
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

; Procedure for sum of cubes of integers from a to b
(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

; Procedure for sum of terms in series (1/(1*3)), (1/(5*7)), (1/(9*11)),..., which coverges to pi/8
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

; Templated abstraction of the above three procedures - essentially, a sigma-summation template
(define (sum term a next b) ; This is basically (define (sum <how to get the current term from a> <how to get the next term from a> <b>))
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; Example usage for cube summation with a helper procedure that increments an argument by 1
(define (inc n)
  (+ n 1))
(define (template-sum-cubes a b)
  (sum cube a inc b))

; Example usage for integer summation with a helper procedure that is the identity function
(define (identity x)
  x)
(define (template-sum-integers a b)
  (sum identity a inc b))

; Example usage for pi-sum with a helper
(define (template-pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

; Using sum template for integral approximation
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)



; Examples of lambda functions
(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))

; Procedure for pi-sum using a lambda
(define (lambda-pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

; Procedure for integral using a lambda
(define (lambda-integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))



; Example of using let to declare local variables
; The let special form is just syntactic sugar for using lambda inside of a procedure to artificially (or authentically?) create local variables
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

; Example of using define for local variables, instead of let. This is not preferred/conventional
(define (f-defines x y)
  (define a (+ 1(* x y)))
  (define b (-1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))



; Half-interval method of finding roots of an equation
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

; Wrapper that calls search with the appropriate positive and negative points in the right place
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
              (search f a b))
             ((and (negative? b-value) (positive? a-value))
              (search f b a))
             (else
              (error "Values are not of opposite sign" a b)))))

; Estimating root of sin between 2 and 4, which is pi
(half-interval-method sin 2.0 4.0)

; Estimating the root of a polynomial function
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)



; Estimating fixed points of a function
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

; Approximate the fixed point of the cosine function starting with an initial guess of 1
(fixed-point cos 1.0)

; Using fixed point to find a solution to the equation y = sin(y) + cos(y)
(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)

; Using fixed point procedure to approximate square roots
(define (fixed-pt-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
