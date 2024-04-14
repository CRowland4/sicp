#lang scheme

; 1.1 - Done in Liquid Text - figuring out what the output of some lines of code would be



; 1.2 - Done in Liquid Text - translating a standard mathematical expression to prefix notation



; 1.3
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-square-largest-two x y z) ; Final solution
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))



; 1.4 - Describe the behavior of the procedure below
#|
(define (a-plus-abs-b ab)     Commented out so the lack of definition for a and b doesn't throw an error
  ((if (> b 0) + -) a b))
|#

; If b is greater than 0, return the evaluation of a + b. Otherwise, return the evaluateion of a - b. The actual operators + and - are the result of the evaluation of the if statement.



; 1.5 - Describe the result of running the code below with both a normal-order evaluation interpreter, and an applicative-order evaluation interpreter.
(define (p)
  (p))
(define (test x y)
  (if (= x 0) 0 y))

;(test 0 (p))  This line is part of the question, but I've commented it out to avoid crashing this code when it's run.

#|
Initial thoughts:
For normal-order evaluation, I expect this to throw an error of some sort - the full expansion of the expression would cause an infinite recursive loop
For applicative-order evaluation, I would expect the result to give me the procedure p

I was wrong.
Normal-order evaluation works fine; since there are no non-primitive operators in the expression, test is evaluated to 0. The value of (p) isn't needed, so p is never called,
    avoiding the recursive loop.
Since applicative-order evaluation first evaluates the operator AND operands before applying the procedure to the arguments, (p) is evaluated. The result of the expression (p) is
    the return value of p, which is (p), and on and on.
|#



; 1.6 - What happens when you try to run the below sqrt-iter procedure with the newly defined new-if procedure?

#|
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
                                                   Commented out so the lack of definition of the helper functions doesn't throw an error
(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
     guess
     (new-sqrt-iter (improve guess x) x)))
|#

#| This would cause an infinite recursive loop. The `if` special form only evaluates whichever of the cosequent and alternative are required by the result of evaluating the predicate,
     so if the guess is good enough, the next recursive iteration isn't called.
But the new-if procedure is just a procedure with the applicative-order evaluation rules, so there isn't any "short-circuiting" to stop evaluating the consequent when the predicate
     is true. So the consequent is always evaluated, which creates the infinite recursive loop
|#



; 1.7 - Explain why the good-enough procedure won't work very well as a check for the square roots of very large and very small numbers, and design a new sqrt procedure that
;           considers the ratio of the old guess to the new guess.
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

; With a very small number, the set limit of 0.001 may not be small enough to determine if a guess is "good" or not. For example, if we want to estimate the square root of 0.0004, this
;     function would say that the guess 0.0008 is "good enough", because the difference between the square of that guess and 0.0004 is less than the set limit of 0.001. But of course
;     it's ridiculously to say that doubling a number is a good enough estimate of it's square root.
(good-enough? 0.0008 0.0004)

; For large numbers, if the number is too big, the rounding errors produced when squaring the guess (due to how computers handle floating-point numbers)
;     will be greater than 0.001, so the program will never terminate.
; It doesn't appear to be too much of an issue with current computing power, at least for a basic sqrt function of this nature, given that it still works on my personal laptop with
;     numbers that had nearly 200 digits, which is far larger than anything that would be reasonably calculated in a program.

; New method for a sqrt calculator that uses the difference between guesses to determine when to stop.
(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x) x)))
  
(define (improve guess x)
    (average guess (/ x guess)))


(define (average x y)
    (/ (+ x y) 2))

; Stop when change is a very small fraction of the guess
(define (new-good-enough? guess x)
    (< (/ (abs (- (improve guess x) guess)) guess) 0.00000001))

(define (sqrt x)
  (new-sqrt-iter 1.0 x))



; 1.8 - For the cube-root approximation procedure, we use the same structure, and will only have to rewrite the improve procedure
(define (cube-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-iter (improve-cube guess x) x)))

(define (cube-good-enough? guess x)
    (< (/ (abs (- (improve-cube guess x) guess)) guess) 0.00000001))

(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root x)
  (cube-iter 2.0 x))
