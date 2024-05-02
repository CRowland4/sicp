#lang scheme
; Everything between here and the row of semi-colons is defined for access, and isn't in the order in which it's presented in the book
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define nil '())




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Linear combination where all arguments are number
(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

; Example of what a more abstract linear combination would look like, where a b x and y didn't necessarily have to be numbes
#|
(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))
|#

; Here, add and mul would be "trusted" to handle the inputs appropriately depending on their data types, and the procedure linear-combination doesn't
;    have to be concerned with the types of its inputs



; Basic arithmetic procedures for rational numbers, though we don't yet have an implementation of a "rational number" yet
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
                (* (numer y) (denom x)))
             (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
                (* (numer y) (denom x)))
             (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
             (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))



; A way to "glue" two integers together using cons (which makes a "pair", a compound data structure), and then using car and cdr to extract the two pieces
; "cons" stands for "Construct", "car" stands for "Contents of Address part of Register", and "cdr" stands for "Contents of Decrement part of Register"
; Footnote in the book explains where these names come from
(define rat (cons 1 2))
(car rat)
(cdr rat)

; The cons procedure can be used with existing pairs
(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(car (car z))
(car (cdr z))



; Finishing the rational number implementation using cons, car, and cdr
(define (make-rat-no-reduction n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))



; This provides us with a way to display the rational numbers
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))



; Trying out procedures on rational numbers
(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))



; Using gcd to modify make-rat to reduce the rational number to lowest terms
#|
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))          Commented out because it's defined at the top to make it available to everything in the file
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
|#
(print-rat (add-rat one-third one-third))



; Alternate way of reducing rational numbers to lowest terms
(define (make-rat-alt n d) (cons n d))
(define (numer-alt x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))
(define (denom-alt x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))



; Implementation of cons, car, and cdr using only procedures and no data structures
(define (cons-modified x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (car-modified z) (z 0))
(define (cdr-modified z) (z 1))



; Manual creation of the "list" (1, 2, 3). We use " '() " instead of the nil keyword, because apparently nil is no longer used in Scheme.
(cons 1
      (cons 2
            (cons 3 '())))

; The same list, but using the "list" primitive. Equivalent to the above definition, but obviously much easier to use
(list 1 2 3)
(define one-through-four (list 1 2 3 4))
one-through-four
(car one-through-four)  ; 1
(cdr one-through-four)  ; (2 3 4)
(car (cdr one-through-four))  ; 2     This could also be (cadr one-through-four). Each "a" between "c" and "r" is a call to "car", and each "d" is a call to "cdr"
(cons 10 one-through-four)  ; (10 1 2 3 4)
(cons 5 one-through-four)  ; (5 1 2 3 4)



; Accessing items from a list by their position (or "index", but that word hasn't been used yet)
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)



; Procedure for the length of a list
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define odds (list 1 3 5 7))
(length odds)

; Iterative implementation of length
(define (length-iter items)
  (define (iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+1 count))))
  (length-iter items 0))



; Procedure for appending one list two another
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))



; Scaling each number in a list by a given factor
(define (scale-list-old items factor)  ; Old because it's redefined later
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list-old (cdr items)
                        factor))))
(scale-list-old (list 1 2 3 4 5) 10)



; The map procedure, that applies a given procedure to every element in a given list (the real map implementation is more general)
(define (one-list-map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (one-list-map proc (cdr items)))))
(map abs (list -10 2.5 -11.6 17))
(map (lambda (x) (* x x)) (list 1 2 3 4))

; Procedure scale-list in terms of map
(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))
(scale-list (list 1 2 3 4 5) 10)



; Procedure for counting leaves of a tree
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define foo (cons (list 1 2) (list 3 4)))
(length foo)  ; 3
(count-leaves foo)  ; 4
(list foo foo)
(length (list foo foo))  ; 2
(count-leaves (list foo foo))  ; 8
(length (cons 1 (list 2)))  ; 2
(count-leaves (cons 1 (list 2)))  ; 2



