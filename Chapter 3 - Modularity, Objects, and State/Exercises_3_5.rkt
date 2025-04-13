#lang sicp
; Everything between here and the row of semi-colons is defined for access, and isn't in the order in which it's presented in the book

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-line x) (newline) (display x))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (show x)
  (display-line x)
  x)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (add-streams s1 s2) (stream-map-generalized + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (square x)
  (* x x))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 3.50
(define (stream-map-generalized proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-generalized
              (cons proc (map stream-cdr argstreams))))))



; Exercise 3.51
; a.)
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))  ;  0
#|
After printing 0, because (show 0) is called during the defining of x, my initial answer is that the following is x:

              (0 . (delayed stream-map show (stream-cdr (delayed stream-enumerate-interval 1 10))))

I was right, basically, but the interpreter simplye displays '(0 . #<promise>)'
|#

; b.)
(stream-ref x 5)
#|
My initial solution. Since x is defined as (0 . #<promise>), and we're not explicitly showing x, it shouldn't be
printed here. (Well, it shouldn't be printed again after the initial printing of 0 when x is defined).
(1 . #<promise>)
(2 . #<promise>)
(3 . #<promise>)
(4 . #<promise>)
(5 . #<promise>)

I forgot that show was an argument of stream-map, and isn't mapping the whole promise. So this actually prints

1
2
3
4
55

The extra 5 is the actual returned value of the (stream-ref x 5).
|#

; c.)
(stream-ref x 7)
#|
Based on the previous exercise, this should display

1
2
3
4
5
6
77

So this is right when evaluated alone, but when evaluated AFTER (stream-ref x 5), this only outputs

6
77

which makes since because x is a stream, and has already gone through the first 5 numbers with (stream-ref x 5).
|#



; Exercise 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

(stream-ref y 7)
(display-stream z)

#| a)

; 1
(stream-ref y 7)

; 2
(stream-ref (stream-filter even? seq)
            7)

; 3
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 1 20)))
            7)

; 4
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (cons-stream
                                        1
                                        (stream-enumerate-interval 2 20))))
            7)

; 5
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (1 . (delay (stream-enumerate-interval 2 20)))))
            7)

; 6
(stream-ref (stream-filter even?
                           ((accum 1) . (delay (stream-map accum
                                                           (stream-cdr
                                                            (1 . (delay (stream-enumerate-interval 2 20))))))))
            7)

; 7 -- after this step, sum is 1
(stream-ref (stream-filter even?
                           ((accum 1) . (delay (stream-map accum
                                                           (stream-enumerate-interval 2 20)))))
            7)

; 8
(stream-ref (stream-filter even?
                           (1 . (delay (stream-map accum
                                                   (stream-enumerate-interval 2 20)))))
            7)

; 9
(stream-ref (stream-filter even? (stream-cdr (1 . (delay (stream-map accum
                                                   (stream-enumerate-interval 2 20))))))
            7)

; 10 - This is identical to step 3, but now our enumerate-interval has been incremented by 1
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 2 20)))
            7)

; 11
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (cons-stream
                                        2
                                        (stream-enumerate-interval 3 20))))
            7)

; 12
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (2 . (delay (stream-enumerate-interval 3 20)))))
            7)

; 13
(stream-ref (stream-filter even?
                           (cons-stream (accum 2)
                                        (stream-map accum
                                                    (stream-cdr (2 . (delay (stream-enumerate-interval 3 20)))))))
            7)

; 14 - after this step, sum is 3
(stream-ref (stream-filter even?
                           ((accum 2) . (delay (stream-map accum
                                                           (stream-eneumerate-interval 3 20)))))
            7)

; 15
(stream-ref (stream-filter even?
                           (3 . (delay (stream-map accum
                                                   (stream-enumerate-interval 3 20)))))
            7)

; 16
(stream-ref (stream-filter even?
                           (stream-cdr (delay (stream-map accum
                                                   (stream-enumerate-interval 3 20)))))
            7)

; 17
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 3 20)))
            7)

; Now I see the pattern and we can start to generalize:
; <sum> -- <low part of enumerate-interval>
; 6 -- 4   This is actually the first even number, as the previous 2 were 1 and 3
; 10 -- 5
; 15 -- 6
; 21 -- 7
; 28 -- 8
; 36 -- 9
; 45 -- 10
; 55 -- 11
; 66 -- 12
; 78 -- 13
; 91 -- 14
; 105 -- 15
; 120 -- 16
; 136 -- 17
; 153 -- 18
; 171 -- 19
; 190 -- 20
; 210 -- 21    Finished, because 21 > 20

; So the value of sum is 210, and the 7th indexed element of the filter process is 136

; Turns out I was right about the 7th index being 136, but from step 1 I started simplifying wrong because
;  I started evaluating y befor substituting y and 7 into stream-ref, so I calculated sum incorrectly. Corrected version below.

; 1
(stream-ref y 7)

; 2
(stream-ref (stream-cdr y)
            6)

; 3
(stream-ref (stream-cdr (stream-cdr y))
            5)

; 4
(stream-ref (stream-cdr (stream-cdr (stream-cdr y)))
            4)

; 5
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))
            3)

; 6
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y)))))
            2)

; 7
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))))
            1)

; 8
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y)))))))
            0)

; 9
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))))))

; 10
(car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))))))

; And now I see my mistake - in my initial set of steps, I didn't actually begin incorrectly. Steps 1-17 are correct,
;   as are this most recent set of steps 1-11. The mistake was in my comment that I see the pattern. What I had seen
;   was the pattern for odd numbers going through the filter check, but if I had gone through one more iteration,
;   I would have hit the first even number: 6. This would have resulted in a pair being returned from the call to
;   filter, rather than just the result of another call to filter. So below I pic up where I left off from the
;   initial set of steps.

; 17
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 3 20)))
            7)

; ... 18
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (cons-stream
                                        3
                                        (stream-enumerate-interval 4 20))))
            7)

; 19
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (3 . (delay (stream-enumerate-interval 4 20)))))
            7)

; 20 - sum is 6 after this step
(stream-ref (stream-filter even?
                           (cons-stream (accum 3)
                                        (stream-map accum
                                                    (stream-enumerate-interval 4 20))))
            7)

; 21
(stream-ref (stream-filter even?
                           (6 . (stream-map accum
                                            (stream-enumerate-interval 4 20))))
            7)

; 22 - and this is where things change, because the even? predicate will return true now
(stream-ref (cons-stream 6
                         (stream-filter
                          even?
                          (delay (stream-map accum
                                            (stream-enumerate-interval 4 20)))))
            7)

; 23 - There is not a delay behind the next call to stream-filter, so instead of doing another round of the filter,
;        we back up even further and do a round of stream-ref, which I didn't do before incorrectly generalizing in
;        in my first set of steps.
(stream-ref (6 . (delay (stream-filter
                         even?
                         (delay (stream-map accum
                                            (stream-enumerate-interval 5 20))))))
            7)

; 24
(stream-ref (stream-cdr (6 . (delay (stream-filter
                         even?
                         (delay (stream-map accum
                                            (stream-enumerate-interval 5 20)))))))
            6)

; Now it's clear to see that ever even number passing throug filter will decrement the ref index by one, meaning the
;  process will be stopped after the 7th indexed even number, which will subsequently stop the accumulation. So it makes
;  sense now that sum will only be 136 after this process, not 210.


b)

; 1
(display-stream z)

; 2
(stream-for-each display-line z)

; 3
(stream-for-each display-line
                 (stream-filter (lambda (x) (= (remainder x 5) 0))
                                seq))

; 4
(stream-for-each display-line
                 (stream-filter (lambda (x) (= (remaiinder x 5) 0))
                                (stream-map accum
                                            (stream-enumerate-interval 1 20))))

; Now there's no 7th index (or any index) that will stop the process at any point, so the enumeration will actually happen through 20.
;  But since in the problem this is called after (stream-ref y 7), the sum will start at 136, rather than at 0. So once again the numbers
;  will be as follows:
; <sum> -- <low part of enumerate-interval>
; 136 - 1
; 137 - 2
; 139 - 3
; 142 - 4
; 146 - 5
; 151 - 6
; 157 - 7
; 164 - 8
; 172 - 9
; 181 - 10
; 191 - 11
; 202 - 12
; 214 - 13
; 227 - 14
; 241 - 15
; 256 - 16
; 272 - 17
; 289 - 18
; 307 - 19
; 326 - 20
; 346 - 21

; No numbers here are divisible by 5, so nothing will be printed to the screen, and the sum will be 346;

Note that these numbers assume we are NOT using the memoization. With memoization, the two calls would not both add to sum,
  because the values generated for the stream by the first call would be stored, and when called, those values would be returned
  without executing the delayed procedure and adding to sum. With memoization, 10, 15, 45, 55, 105, 120, 190, and 210 would be
  printed to the screen, and sum would be 210.

I believe I also could have missed something here since I did not consider the calculation steps that would be taken upon
  defining the structures used for the problem. While that will change the value of the outcome, it won't change the mechanics
  of the problem, so I don't feel the need to redo the whole thing to correct that bit
|#



; Exercise 3.53
(define s (cons-stream 1 (add-streams s s)))
; This stream will generate: 1, 2, 4, 8, 16 ...



; Exercise 3.54
(define (mul-streams s1 s2) (stream-map-generalized * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (add-streams integers ones))))



; Exercise 3.55
(define (partial-sums s)
  (add-streams (cons-stream 0 (partial-sums s)) s))



; Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))



; Exercise 3.57
#|
Reference procedures
(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map-generalized proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-generalized
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2) (stream-map-generalized + s1 s2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(cons-stream a b) is (cons a (delay b))


--- Calculating the 3rd (n = 3) Fibonacci, 1
Fibs, expanded:

(0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

Now we can start the calculation:
(stream-ref fibs 3)

-> (stream-ref
       (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
       3)

-> (stream-ref
       (cons-stream 1 (add-streams (stream-cdr fibs) fibs))
       2)

-> (stream-ref
       (1 . (delay (add-streams (stream-cdr fibs) fibs)))
       2)

-> (stream-ref
       (add-streams (stream-cdr fibs) fibs)
       1)

-> (stream-ref
       (add-streams
           (cons-stream 1 (add-streams (stream-cdr fibs) fibs))
           (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
       1)

-> (stream-ref
       (add-streams
           (1 . (delay (add-streams (stream-cdr fibs) fibs)))
           (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
       1)

-> (stream-ref
       (stream-map-generalized
           +
           (1 . (delay (add-streams (stream-cdr fibs) fibs)))
           (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
       1)

-> (stream-ref
       (cons-stream
           (apply + (1 0))
           (apply
               stream-map-generalized
               (cons +
                     (add-streams (stream-cdr fibs) fibs)
                     (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
       1)

-> (stream-ref
       (cons-stream
           1
           (apply
               stream-map-generalized
               (cons +
                     (add-streams (stream-cdr fibs) fibs)
                     (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
       1)

-> (stream-ref
       (1 . (delay (apply
               stream-map-generalized
               (cons +
                     (add-streams (stream-cdr fibs) fibs)
                     (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
       1)

-> (stream-ref
       (apply
           stream-map-generalized
           (cons +
                 (add-streams (stream-cdr fibs) fibs)
                 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
       0)

-> (stream-ref
       (apply
           stream-map-generalized
           (cons +
                 (add-streams
                      (1 . (add-streams (stream-cdr fibs) fibs))
                      (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
                 (1 . (delay (add-streams (stream-cdr fibs) fibs)))))
       0)

-> (stream-ref
       (apply
           stream-map-generalized
           (cons +         
                 (stream-map-generalized
                      +
                      (1 . (add-streams (stream-cdr fibs) fibs))
                      (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs)))))
                 (1 . (delay (add-streams (stream-cdr fibs) fibs)))))
       0)

-> (stream-ref
       (apply
           stream-map-generalized
           (cons +  
                 (cons-stream
                      (apply + (map stream-car
                                    (1 . (add-streams (stream-cdr fibs) fibs))
                                    (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))))
                      (apply stream-map-generalized
                             (cons + (map stream-cdr
                                          (1 . (add-streams (stream-cdr fibs) fibs))
                                          (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))))))
                 (1 . (delay (add-streams (stream-cdr fibs) fibs)))))
       0)

-> (stream-ref
       (apply
           stream-map-generalized
           (cons +  
                 (cons-stream
                      (apply + (1 0))
                      (apply stream-map-generalized
                             (cons + (map stream-cdr
                                          (1 . (add-streams (stream-cdr fibs) fibs))
                                          (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))))))
                 (1 . (delay (add-streams (stream-cdr fibs) fibs)))))
       0)

-> (stream-ref
       (apply
           stream-map-generalized
           (cons +  
                 (1 .
                      (apply stream-map-generalized
                             (cons + (map stream-cdr
                                          (1 . (add-streams (stream-cdr fibs) fibs))
                                          (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))))))
                 (1 . (delay (add-streams (stream-cdr fibs) fibs)))))
       0)

-> (stream-ref
       (stream-map-generalized
           (+  
            (1 . (apply stream-map-generalized
                             (cons + (map stream-cdr
                                          (1 . (add-streams (stream-cdr fibs) fibs))
                                          (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))))))
            (1 . (delay (add-streams (stream-cdr fibs) fibs)))))
       0)

-> (stream-car (stream-map-generalized
      (+  
       (1 . (apply stream-map-generalized
                             (cons + (map stream-cdr
                                          (1 . (add-streams (stream-cdr fibs) fibs))
                                          (0 . (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))))))
       (1 . (delay (add-streams (stream-cdr fibs) fibs))))))

-> (stream-car (cons-stream
       (apply + (1 1))
       .... rest doesn't matter cause we're taking cons-stream))

-> 2, took 3 additions. But onoe of the additions was run twice (+ 1 0),
      as a result of (delay (cons-stream 1 (add-streams (stream-cdr fibs) fibs))),
      which would have been avoided with the memo-proc optimization, which reduces the
      number of additions down to 2, or n - 2.

      With memo-proc, to calculate Fib(n) we'll need n - 1 additions.
      Without memo-proc, Fib(n) requires (Fib(n-1) + Fib(n-2)) calculations,
      which turns into the classic brute-force fibonacci calculation, whose number
      of additions increases exponentially.

I made a mistake in this sequence of substitutions by not recognizing immediately that
  a (delay (proc)) had been repeated, and I did it all by hand twice, accidentally demonstrating
  why the memo optimization of delay is so useful.

|#



; Exercise 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))
(define test (expand 1 7 10))  ; Gives 1, 4, 2, 8, 5, 7, 1, 4, 2, 8, 5, 7, on a cycle
(define test2 (expand 3 8 10))  ; Gives 3, 7, 5, then all 0s

; This procedure is giving the decimal digits of the operation (num / den), in base radix



; Exercise 3.59
; a)
(define (div-streams s1 s2) (stream-map-generalized / s1 s2))
(define (integrate-series s)
  (mul-streams s (div-streams ones integers)))

; b)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 (integrate-series sine-series)))
(define sine-series (cons-stream 0 (scale-stream -1 (integrate-series cosine-series))))



; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2)
                                          (stream-car s1))
                            (mul-series (stream-cdr s1)
                                         s2))))



; Exercise 3.61
(define (invert-unit-series S)
  (cons-stream 1
               (scale-stream (mul-series S (invert-unit-series S))
                             -1)))



; Exercise 3.62 - guide that I'm using to check my solutions against (wizardbook.wordpress.com) uses scale-stream and
;                   standard division in his solution, I'm not sure why. I'm using this recent handful of problems as
;                   more of a mental exercise than anything and am not terribly concerned with 100% accuracy, as I've
;                   never been terribly fond of power series.
(define (div-series s1 s2)
  (if (eq? (stream-car s2) 0)
      (display "Error! s2 starts with a 0 term")
      (mul-series s1 (invert-unit-series s2))))

(define tan-series (div-series sine-series cosine-series))



; Exercise 3.63

#|
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))


|#
; Original Version of sqrt-stream, with local variable <guesses>

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

#|
Louis Reasoner's version of sqrt-stream, without local variable <guesses>

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))

In Louis Reasoner's version, the stream passed to stream-map is an expression that creates a stream, which
  is then passed into the stream map. In the original version with <guesses>, the stream passed into stream-map
  is the existing stream <guesses>, so there is no stream construction happening on every calculation.

Since the issue here is the recreation of a stream on every calculation, rather than a memoization process being
  skipped, the original sqrt-stream procedure with <guesses> as a local variable will be more efficient, with or
  without the memo-proc procedure.

NOTE: From reading around other online solutions, it seems that the *reason* that the recursive call to sqrt-stream in Louis'
  version is inefficient is precisely because that reconstruction doesn't benefit from memoization. This makes sense, as a
  freshly created stream won't have the memory of a past stream. In other words, it will have a memo, but that memo will be
  empty. In this case, my second answer above is incorrect. The very reason that the version with <guess> defined locally works
  is because it's memo is saved and reused.
|#



; Exercise 3.64
(define (stream-limit s t)
  (if (< (abs (- (stream-car s) (stream-car (stream-cdr s)))) t)
      (stream-car (stream-cdr s))
      (stream-limit (stream-cdr s) t)))



; Exercise 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream  ; Sequence 1
  (scale-stream (partial-sums (ln2-summands 1)) 1))

(euler-transform ln2-stream)  ; Sequence 2

(accelerated-sequence euler-transform ln2-stream)  ; Sequence 3

; Not sure what the book means by 'how rapidly do these sequences converge,
;   but each converges faster than the previous, as expected.



; Exercise 3.66
#|
Came up with a recursive formula here, not a precise mathematical operation expression but I'm
  happy with it.
|#
(define (pair-position i j)
  (cond ((and (= 1 i) (= j 1)) 1)
        ((= i 1) (* 2 (- j 1)))
        (else (+ (* 2 (pair-position (- i 1) (- j 1))) 1))))

; Ended up using the previous formula to come up with an exact expression.
; Calculations in LiquidText. Needed a floor function to make it work.
(define (calculated-pair-position i j)
  (- (+ (* (expt 2 (+ i (floor (/ i j))))
           (- j i))
        (expt 2 (- (+ i (floor (/ i j))) 1)))
     1))

; The pair (1, 100) is number 198
; The pair (99, 100) is number 950,737,950,171,172,051,122,527,404,031
; The pair (100, 100) is number 1,267,650,600,228,229,401,496,703,205,375



; Exercise 3.67
; Give that we now have the <interleave> I figure I'll make a stream of the other half of the
;  'pair pyramid' we've used in the book, and then interleave that one and the original.

; Used to make the stream of pairs where i <= j
(define (upper-pairs s t)  
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (upper-pairs (stream-cdr s) (stream-cdr t)))))

; Used to make the stream of pairs where i > j
(define (lower-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (lower-pairs (stream-cdr s) (stream-cdr t)))))

; Stream of pairs where i <= j
(define upper-pyramid (upper-pairs integers integers))

; Stream of pairs where i > j
(define lower-pyramid (lower-pairs (add-streams integers ones) integers))

; Stream of all pairs (i, j)
(define all-pairs (interleave upper-pyramid lower-pyramid))



; Exercise 3.68
(define (louis-pairs-procedure s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (louis-pairs-procedure (stream-cdr s) (stream-cdr t))))

;(define louis-upper-pairs (louis-pairs-procedure integers integers))

#|
Explanation, starting withe a first simplification of <louis-pairs-procedure> called with <integers integers>:
(define (louis-pairs-procedure integers integers)
  (interleave
   (cons-stream (proc (stream-car integers))
                (stream-map proc (stream-cdr integers)))
   (louis-pairs-procedure (stream-cdr integers) (stream-cdr integers))))

->
(define (louis-pairs-procedure integers integers)
  (interleave
   (cons-stream (list (stream-car integers) (stream-car integers))
                   (stream-map proc (stream-cdr integers)))
   (louis-pairs-procedure (stream-cdr integers) (stream-cdr integers))))

->
(define (louis-pairs-procedure integers integers)
  (interleave
   ((1 1) . (delay (stream-map proc (stream-cdr s))))                          ; We have a pair with a delayed cdr here, as a result of the cons-stream in stream-map
   (louis-pairs-procedure (stream-cdr integers) (stream-cdr integers))))       ; Now we move on to the second argument of interleave


But this is where Louis' procedure differs - we have to keep evaluating recursively, which will produce
  another pair with a delayed cdr interleaved with a recursive call to louis-pairs-procedure, which will
  produce  another pair with a delayed cdr interleaved with a recursive call to louis-pairs-procedure....
  and so on. Using the first pairs separately with cons-stream allowed us to delay evaluation of the recursive
  call. Then, when the recursive call is forced, the evaluation of the recursive has it's own recursive call,
  which is also delayed.

Essentially, starting with cons-stream and the first pair allows us to evaluate create a sort of gear that catches
  on the next tooth (recursive evaluation), rather than a wheel that just spins out of control with no teeth on
  which to catch. Kinda an odd metaphor but that's what comes to mind.
|#



; Exercise 3.69

#|

Fairly certain this wasn't the 'intended' method the authors had in mind, using interleave and pairs and what not,
  but it works. I've included the 'intended' solution (or so I'm guessing) that I found at wizardbook.wordpress.com
  for reference.
|#
(define (triples s t u)  ; Procedure to build triplets
  (define (triples-inner i j k)
    (cons-stream
     (list (stream-ref s i) (stream-ref t j) (stream-ref u k))
     (cond ((and (= i j) (= j k))
            (triples-inner 0 0 (+ k 1)))
           ((= i j)
            (triples-inner i (+ j 1) k))
           ((= j k)
            (triples-inner (+ i 1) (+ i 1) k))
           (else (triples-inner i (+ j 1) k)))))
  (triples-inner 0 0 0))

; The stream of triplets
(define triples-stream (triples integers integers integers))

; The predicate to use in the filter
(define (pythagorean-triple-checker triple)
  (= (+ (square (car triple))
        (square (cadr triple)))
     (square (caddr triple))))

; Stream of pythagorean trip
(define pythagorean-triples-stream
  (stream-filter pythagorean-triple-checker triples-stream))

; 'Intended' solution
(define (triples-intended s t u)
  (cons-stream
   (list
    (stream-car s)
    (stream-car t)
    (stream-car u))
   (interleave
    (stream-map
     (lambda (x) (append (list (stream-car s)) x))
     (stream-cdr (pairs t u)))
    (triples-intended
     (stream-cdr s)
     (stream-cdr t)
     (stream-cdr u)))))



; Exercise 3.70
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 (else     ; We can skip the > condition because the footnote assures us that we won't have two different pairs with equal weights
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight))))))))

(define (weighted-pairs s t weight-function)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight-function)
    weight-function)))

#|
I'm not sure how to interpret the fact that the problem says <weighted-pairs> should receive "a procedure that computes a weighting function",
  so I'm just going to pass it a lambda function that does what we want.
|#

; a
(define a-stream
  (weighted-pairs integers integers (lambda (pair) (+ (car pair) (cadr pair)))))

; b
(define b-stream
  (stream-filter (lambda (pair) (not (or (= (remainder (car pair) 2) 0)
                                         (= (remainder (cadr pair) 2) 0)
                                         (= (remainder (car pair) 3) 0)
                                         (= (remainder (cadr pair) 3) 0)
                                         (= (remainder (car pair) 5) 0)
                                         (= (remainder (cadr pair) 5) 0))))
                 (weighted-pairs integers integers (lambda (pair) (+ (* 2 (car pair))
                                                                     (* 3 (cadr pair))
                                                                     (* 5 (car pair) (cadr pair)))))))



; Exercise 3.71
(define (rama-weight pair)
  (+ (* (car pair) (car pair) (car pair))
     (* (cadr pair) (cadr pair) (cadr pair))))

(define rama-weighted-pairs-stream
  (weighted-pairs integers integers rama-weight))

(define (rama-stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc s)  ; Slight modification to stream-map here so the procedure runs on s, rather than (stream-car s)
                   (rama-stream-map proc (stream-cdr s)))))

(define potential-rama-pairs-stream
  (rama-stream-map (lambda (s) (list (stream-car s) (stream-car (stream-cdr s))))
                   rama-weighted-pairs-stream))

(define ramanujan-numbers-stream
  (stream-map
   (lambda (dos) (rama-weight (car dos)))
   (stream-filter (lambda (dos) (= (rama-weight (car dos)) (rama-weight (cadr dos))))
                  potential-rama-pairs-stream)))  ; First 6 are 1729, 4104, 13832, 20683, 32832, 39312


   
   