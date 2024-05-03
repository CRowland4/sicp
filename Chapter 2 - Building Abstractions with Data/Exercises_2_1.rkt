#lang scheme
; Procedures defined between here and the separator are here so that the exercises can access them
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (square x) (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (= (remainder x 2) 1))

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

; Exercise 2.1
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
         (if (or (and (< n 0) (< d 0))
          (and (> n 0) (> d 0)))
      (cons (abs (/ n g)) (abs (/ d g)))
      (cons (- (abs (/ n g))) (abs (/ d g))))))



; Exercise 2.2
(define (midpoint-segment s)
  (let ((avg-x (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2))
        (avg-y (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))
    (make-point avg-x avg-y)))
(define (make-segment p1 p2) (cons p1 p2))  ; Segment constructor, takes two points
(define (start-segment s) (car s))  ; Segment selector, takes a segment and returns the start point
(define (end-segment s) (cdr s))  ; Segment selector, takes a segment and returns the end point
(define (make-point x y) (cons x y))  ; Point constructor, takes two integers representing the x and y coordinates
(define (x-point p) (car p))  ; Point selector, takes a point and returns the x coordinate of the point
(define (y-point p) (cdr p))  ; Point selector, takes a point and retrns the y cooridnate of the point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display y-point p)
  (display ")"))



; Exercise 2.3
; Add some helper procedures with points, line segments, and rational numbers
; Distance between two points
(define (point-distance p1 p2)
  (sqrt (+ (square (- (x-point p1)
                      (x-point p2)))
           (square (- (y-point p1)
                      (y-point p2))))))
; Length of a line segment
(define (segment-length s)
  (point-distance (end-segment s) (start-segment s)))
; "Rise" of a line segment
(define (segment-rise s)
  (- (y-point (end-segment s)) (y-point (start-segment s))))
; "Run" of a line segment
(define (segment-run s)
  (- (x-point (end-segment s)) (x-point (start-segment s))))
; Slope of a line segment
(define (segment-slope s)
  (if (= (segment-run s) 0)
      +inf.0
      (/ (segment-rise s) (segment-run s))))
; Reciprocol of a number
(define (reciprocal x)
  (/ 1 x))
; Are line segments perpendicular
(define (segments-perpendicular? s1 s2)
  (or (and (= +inf.0 (segment-slope s1))
           (= 0 (segment-slope s2)))
      (and (= +inf.0 (segment-slope s2))
           (= 0 (segment-slope s1)))
      (= (* -1 (reciprocal (segment-slope s1))) (segment-slope s2))))
; Selectors
(define (side1 r)
  (car r))
(define (side2 r)
  (make-segment (end-segment (car r)) (end-segment (cdr r))))
(define (side3 r)
  (cdr r))
(define (side4 r)
  (make-segment (start-segment (cdr r)) (start-segment (car r))))
(define (length r)
  (let ((distance1 (segment-length (side1 r)))
        (distance2 (segment-length (side2 r))))
    (if (> distance1 distance2)
        distance1
        distance2)))
(define (width r)
  (let ((distance1 (segment-length (side1 r)))
        (distance2 (segment-length (side2 r))))
    (if (< distance1 distance2)
        distance1
        distance2)))
; Rectangle validation - sides must form four 90 degree angles
(define (is-valid-rect? r)
  (and (segments-perpendicular? (side1 r) (side2 r))
       (segments-perpendicular? (side2 r) (side3 r))
       (segments-perpendicular? (side3 r) (side4 r))
       (segments-perpendicular? (side4 r) (side1 r))))
(define (perimeter r)
  (+ (* 2 (length r)) (* 2 (width r))))
(define (area r)
  (* (length r) (width r)))
  
; Constructor of first rectangle implementation - takes 2 line segments that are parallel sides of a rectangle,
;     and returns the pair of segments (s1, s2)
(define (make-rectangle1 s1 s2)
  (let ((rect (cons s1 s2)))
    (if (is-valid-rect? rect)
        rect
        (error "Invalid rectangle"))))
; Test first implementation
(define rec1 (make-rectangle1
               (make-segment (make-point 0 5) (make-point 4 5))
               (make-segment (make-point 0 0) (make-point 4 0))))
(width rec1)
(length rec1)
(perimeter rec1)
(area rec1)

; Constructor of second rectangle implementation - takes 4 points, with the expectation that the connection order is
;     1 -> 2 -> 3 -> 4 -> 1. Returns an the same data shape so that the abstractions barriers are clear.
(define (make-rectangle2 p1 p2 p3 p4)
  (let ((rect (cons (make-segment p1 p2) (make-segment p4 p3))))
    (if (is-valid-rect? rect)
        rect
        (error "Invalid rectangle"))))
; Test second implementation
(define rec2 (make-rectangle2
               (make-point 0 0)
               (make-point 0 5)
               (make-point 4 5)
               (make-point 4 0)))
(width rec2)
(length rec2)
(perimeter rec2)
(area rec2)

; Take away is that there's more to it than is initially obvious to create good abstraction barriers



; Exercise 2.4
#|
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(car (cons x y))
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x
|#
; With this procedural implementation of cons and car, (car (cons x y)) ultimately reduces down to a procedure that always returns x

; Corresponding cdr

#|
(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons x y))
(cdr (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) q))
((lambda (p q) q) x y)
y
|#



; Exercise 2.5
(define (cons-alt a b)
  (* (expt 2 a) (expt 3 b)))
(define (car-alt p)
  (define (two-dividor current count)
    (if (even? current)
        (two-dividor (/ current 2) (+ count 1))
        count))
  (two-dividor p 0))
(define (cdr-alt p)
  (define (three-dividor current count)
    (if (= (remainder current 3) 0)
        (three-dividor (/ current 3) (+ count 1))
        count))
  (three-dividor p 0))


(define test (cons-alt 1496 429))
(car-alt test)
(cdr-alt test)



; Exercise 2.6
; Zero is a procedure that receives as input a procedure f and applies f 0 times to f's own input.
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Simplification as suggested by the exercise
#|
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))  ; So this is one
|#
; One is a procedure that receives as input a procedure f and applies f 1 time to f's own input
(define one (lambda (f) (lambda (x) (f x))))

; Therefore it follows that two is a procedure that receives as input a procedure f and applies f 2 times to f's own input
(define two (lambda (f) (lambda (x) (f (f x)))))

; Addition
(define (church+ f g)
  (lambda (h) (lambda (x) ((g h) ((f h) x)))))



; Extended Exercise (2.1.4) - Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval-old x y)  ; Called "old" because it's rewritten in a later exercise
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval-old
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

; Exercise 2.8
; What we're actually thinking about is the lower bound on the resistance difference of two intervals. The smallest this could be is if the first
;     resistor's resistance value was equal to its lower bound, and if the second resistor's resistance value was equal to its upper bound.
;     Similarly, the max this could be is if the first value was at its maximum and the second value was at its minimum
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9
; Procedure for the width of an interval
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

#|
Process for calculating the width of a sum of two intervals

(interval-width (add-interval x y))
(interval-width (make-interval (+ (lower-bound x) (lower-bound y))
                      (+ (upper-bound x) (upper-bound y))))
(/ (- (+ (upper-bound x) (upper-bound y))
      (+ (lower-bound x) (lower-bound y)))
   2)

Switching to mathematical notation, the above is equivalent to
(up-x + up-y - low-x - low-y) / 2
((up-x - low-x) / 2) + ((up-y - low-y) / 2)
(width-x + width-y)

So the width of the sum of two intervals is the sum of the widths of the two intervals
|#

#|
Process for calculating the width of a difference of two intervals

(interval-width (sub-interval x y)
(interval-width (make-interval (- (lower-bound x) (upper-bound y))
                      (- (upper-bound x) (lower-bound y))))
(/ (- (- (upper-bound x) (lower-bound y))
      (- (lower-bound x) (upper-bound y)))
   2)

And again switching to mathematical notation
(up-x - low-y - low-x + up-y) / 2
((up-x - low-x) / 2) + ((up-y - low-y) / 2)
(width-x + width-y)


So the width of the difference of two intervals is also the sum of the widths of the two intervals
|#

; For multiplication and division, we can't just divide or multiply the widths of the individual intervals.
; Here are two intervals, with widths of 3 and 18 respectively. If these followed the similar patterns, we would expect a width of 54 for the product,
;     and 6 for the solution.
(interval-width (mul-interval-old (make-interval 4 7) (make-interval 5 23)))  ; 70.5
(interval-width (div-interval (make-interval 5 23) (make-interval 4 7)))  ; 2.518

; Exercise 2.10
; We also check for one of the bounds of y being zero, because that is also not able to be calculated (would be division by 0)
(define (div-interval-updated x y)
  (cond ((or (= (upper-bound y) 0)
             (= (lower-bound y) 0))
         (error "Invalid division - one of the bounds of interval y is 0"))
        ((or (and (> (upper-bound x) 0) (< (lower-bound x) 0))
             (and (> (upper-bound y) 0) (< (lower-bound y) 0)))
         (error "Invalid division - one of the intervals spans 0"))
         (mul-interval
          x
          (make-interval (/ 1.0 (upper-bound y))
                         (/ 1.0 (lower-bound y))))))

; Exercise 2.11
; With 4 possible spots (upper and lower bounds of both intervals) each haveing two possible signs (+ or -), we are left with 16 potential sign combinations.
;    However, it is not possible to have an interval with a negative upper bound and a positive lower bound. After removing such intervals, we're left with 9
;    possible sign combinations (which makes sense, because there are 3 valid interval combinations (-, -), (-, +), (+, +), and two spots with three options each gives us 9).
; Then there are 0s to deal with. We can eliminate several cases with an interval that contains a zero:
;     (0, 0), because it isn't an interval
;     (0, -), because this is a "backwards", and therefore invalid, interval
;     (+, 0), for the same reason as above
; So the valid choices for intervals that contain a 0 are (0, +) and (-, 0),
;      and the valid combinations of intervals without a zero are (-, -), (-, +), (+, +).
; Now we need to know how many interval pairs there are where one of the intervals contains a zero, which is a small combinatorics problem.
;     Number of pairs where the first interval has a 0, and the second doesn't: 6
;     Number of pairs where the second interval has a 0, and the first doesn't: 6
;     Number of pairs where both intervals contain a zero: 4
;     This gives us a total of 16 potential pairs where a zero is in at leas one of the intervals.
; Even though we have 16 "new" intervals to consider, each one of those 16 pairs uses a call to make-interval that is already needed by one of the original 9.
;     So we can add in these 16 combinations as an "or" conditions with the pre-existing conditions for the original 9 pairs, meaning we still really only have 9
;     cases in the form of 9 different make-interval calls, where still only one of them requires more than 2 multiplications.
(define (mul-interval x y)
  (cond ((or (and (> (lower-bound x) 0)  ; Case 1: [(+, +), (+, +)] or [(0, +), (0, +)] or [(0, +), (+, +)] or [(+, +), (0, +)]
              (> (upper-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound y) 0))
            (and (= 0 (lower-bound x))
                 (> (upper-bound x) 0)
                 (= 0 (lower-bound y))
                 (> (upper-bound y) 0))
            (and (= 0 (lower-bound x))
                 (> (upper-bound x) 0)
                 (> (lower-bound y) 0)
                 (> (upper-bound y) 0))
            (and (> (lower-bound x) 0)
                 (> (upper-bound x) 0)
                 (= 0 (lower-bound y))
                 (> (upper-bound y) 0)))
         (make-interval (* (lower-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((or (and (> (lower-bound x) 0)  ; Case 2: [(+, +), (-, +)] or [(0, +), (-, 0)] or [(0, +), (-, +)] or [(+, +), (-, 0)]
              (> (upper-bound x) 0)
              (< (lower-bound y) 0)
              (> (upper-bound y) 0))
             (and (= 0 (lower-bound x))
                 (> (upper-bound x) 0)
                 (< (lower-bound y) 0)
                 (= 0 (upper-bound y)))
             (and (= 0 (lower-bound x))
                 (> (upper-bound x) 0)
                 (< (lower-bound y) 0)
                 (> (upper-bound y) 0))
             (and (> (lower-bound x) 0)
                 (> (upper-bound x) 0)
                 (< (lower-bound y) 0)
                 (= 0 (upper-bound y))))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((or (and (< (lower-bound x) 0)  ; Case 3: [(-, +), (+, +)] or [(-, 0), (0, +)] or [(-, 0), (+, +)] or [(-, +), (0, +)]
              (> (upper-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (= 0 (upper-bound x))
                 (= 0 (lower-bound y))
                 (> (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (= 0 (upper-bound x))
                 (> (lower-bound y) 0)
                 (> (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (> (upper-bound x) 0)
                 (= 0 (lower-bound y))
                 (> (upper-bound y) 0)))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (upper-bound y))))
        ((or (and (> (lower-bound x) 0)  ; Case 4: [(+, +), (-, -)] or [(0, +), (-, -)]
              (> (upper-bound x) 0)
              (< (lower-bound y) 0)
              (< (upper-bound y) 0))
             (and (= 0 (lower-bound x))
                 (> (upper-bound x) 0)
                 (< (lower-bound y) 0)
                 (< (upper-bound y) 0)))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (upper-bound y))))
        ((or (and (< (lower-bound x) 0)  ; Case 5: [(-, -), (+, +)] or [(-, -), (0, +)]
              (< (upper-bound x) 0)
              (> (lower-bound y) 0)
              (> (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (< (upper-bound x) 0)
                 (= 0 (lower-bound y))
                 (< (upper-bound y) 0)))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (upper-bound x) (lower-bound y))))
        ((or (and (< (lower-bound x) 0)  ; Case 6: [(-, -), (-, +)] or [(-, 0), (-, 0)] or [(-, 0), (-, +)] or [(-, -), (-, 0)]
              (< (upper-bound x) 0)
              (< (lower-bound y) 0)
              (> (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (= 0 (upper-bound x))
                 (< (lower-bound y) 0)
                 (= 0 (upper-bound y)))
             (and (< (lower-bound x) 0)
                 (= 0 (upper-bound x))
                 (< (lower-bound y) 0)
                 (> (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (< (upper-bound x) 0)
                 (< (lower-bound y) 0)
                 (= 0 (upper-bound y))))
         (make-interval (* (lower-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((or (and (< (lower-bound x) 0)  ; Case 7: [(-, +), (-, -)] or [(-, 0), (-, -)] or [(-, +), (-, 0)]
              (> (upper-bound x) 0)
              (< (lower-bound y) 0)
              (< (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (= 0 (upper-bound x))
                 (< (lower-bound y) 0)
                 (< (upper-bound y) 0))
             (and (< (lower-bound x) 0)
                 (> (upper-bound x) 0)
                 (< (lower-bound y) 0)
                 (= 0 (upper-bound y))))
         (make-interval (* (upper-bound x) (lower-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (< (lower-bound x) 0)  ; Case 8: (-, -), (-, -)
              (< (upper-bound x) 0)
              (< (lower-bound y) 0)
              (< (upper-bound y) 0))
         (make-interval (* (upper-bound x) (upper-bound y))
                        (* (lower-bound x) (lower-bound y))))
        ((and (< (lower-bound x) 0)  ; Case 9: (-, +), (-, +) - This is the case that requires more than two multiplications
              (> (upper-bound x) 0)
              (< (lower-bound y) 0)
              (> (upper-bound y) 0))
         (make-interval (min (* (lower-bound x) (upper-bound y))
                             (* (upper-bound x) (lower-bound y)))
                        (max (* (lower-bound x) (lower-bound y))
                             (* (upper-bound x) (upper-bound y)))))))

; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))
; If p is uncertainty (as a percent tolerance), w is the width of an interval, and c is the midpoint (center) of an interval, then we have the following relationship
;     (according to the paragraph above this exercise): p = w/c -> w = cp.
; So our make-center-percent interval should create the interval with width w, which is [(c - cp), (c + cp)] or [(c(1 - p)), (c(1 + p))]
(define (make-center-percent c p)  ; Based on the wording of the question, I'll assume that p is not yet in decimal form
  (make-interval (* c (- 1 (/ p 100.0)))
                 (* c (+ 1 (/ p 100.0))))) 
(define (percent i)  ; This procedure returns a percentages as a decimal
  (abs (/ (width-interval i) (center i))))
(make-center-percent 5 25)
(percent (make-center-percent 5 25))

; Exercise 2.13 - Done in Liquid Text, all math

; Exercise 2.14
(define (par1 r1 r2)  ; Parallel resistance calculation for Rp = (R1*R2)/(R1+R2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))  ; Parallel resistance calculation for Rp = 1/(1/R2 + 1/R2)
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))
(define a (make-interval 33 34))
(define b (make-interval 145 150))

(par1 a b)  ; (26.005434782608695 . 28.651685393258425)
(center (par1 a b))  ; 27.32856008793356
(percent (par1 a b))  ; 0.04841547820549343

(par2 a b)  ; (26.882022471910112 . 27.717391304347824)
(center (par2 a b))  ; 27.299706888128966
(percent(par2 a b))  ; 0.01529995973694804

(define c (make-interval 389 390))
(div-interval c c)  ; (0.9974358974358974 . 1.0025706940874035)
(center (div-interval c c))  ; 1.0000032957616505
(percent (div-interval c c))  ; 0.0025673898642479777

(par1 a c)  ; (30.275943396226413 . 31.4218009478673)
(center (par1 a c))  ; 30.848872172046857
(percent (par1 a c))  ; 0.01857211416434188

(par2 a c)  ; (30.419431279620856 . 31.27358490566038)
(center (par2 a c))  ; 30.84650809264062
(percent(par2 a c))  ; 0.013845223963021396

; Lem is right - the two equations give different results for the same inputs

(div-interval a a)  ; (0.9705882352941176 . 1.0303030303030303)
(center (div-interval a a))  ; 1.000445632798574
(percent (div-interval a a))  ; 0.029844097995545646

(div-interval b b)  ; (0.9666666666666667 . 1.0344827586206897)
(center (div-interval b b))  ; 1.0005747126436781
(percent (div-interval b b))  ; 0.03388856978747849

(define d (make-interval 3 100))
(div-interval d d)  ; (0.03 . 33.33333333333333)
(center (div-interval d d))  ; 16.681666666666665
(percent (div-interval d d))  ; 0.9982016185433109

; Given the different intervals produced by a/a, b/b, and c/c, we see that the concept of "1" in this realm of intervals isn't clear.

(div-interval a b)  ; (0.22 . 0.23448275862068965)
(center (div-interval a b))  ; 0.22724137931034483
(percent (div-interval a b))  ; 0.031866464339908945

; Clearly Lem is right, and our interval operations don't follow the same rules as standard algebraic operations with numbers

; Exercise 2.15
; Alyssa's conclusion seems to make sense - R1 and R2 both contain "uncertainty", and it is logical to say that reducing the amount of uncertainty
;     in the calculation will reduce the amount of uncertainty in the answer. This plays out in every example below - par2 always produces less
;     uncertainty in its result. By not repeating variables, we don't have to account for the same interva's uncertainty more than once.
(> (percent (par1 a d)) (percent (par2 a d)))  ; true
(> (percent (par1 b d)) (percent (par2 b d)))  ; true
(> (percent (par1 c d)) (percent (par2 c d)))  ; true
(> (percent (par1 b c)) (percent (par2 b c)))  ; true
(> (percent (par1 a c)) (percent (par2 a c)))  ; true
(> (percent (par1 a b)) (percent (par2 a b)))  ; true

; Exercise 2.16
; The explanation for why equivalent algebraic expressions may lead to different answers is given in exercise 2.15.
;     You're "double counting" the uncertainty for a variable each time that variable shows up in the expression more than one time.
; I'm not going to spend the time trying to mathematically prove this one way or another, but I will brainstorm a bit. My gut says that yes, there is a way to do this.
;     My thought is that this would involve doing the calculations themselves with only the centers of the intervals, and then introducing the uncertainty of each interval
;     into the final answer. Each interval would have a value, or data structure, or something, attached to it that somehow represents its uncertainty.
;     Then there would be some mechanism for integrating those uncertainties into the final answer.
; Since the uncertainty of one combination of intervals may differ from the uncertainty of combining the same intervals with a different operation (i.e. I1 + I2 vs I1 / I2),
;     it could be neccessary to have one uncertainty "structure" for each combination of variables in the initial expression. This could be attached to each interval-operation
;     pair, so (+ I1) would be added to the "net" uncertainty if R1 was added to some other value in the problem, or (/ I2) if some value was divided by I2 at some point.
; I did a very small amount of reading into this, and apparently this is related to (or just *is*) the Dependency Problem in mathematics. I'm satisfied with not answering this
;     question explicitly, as it seems like the point is to gain an appreciation and respect for the flexibility and complexity of data abstractions, rather than to come up
;     with a mathematically rigorous solution to a complex problem (I can't even figure out whether or not this problem even HAS been solved yet).
; My "brainstorming" is also potentially way off the mark, since it isn't obvious that it has anything to do with satisfying the properties of an algebraic field, which
;     play heavily into an actual potential solution to this problem.
