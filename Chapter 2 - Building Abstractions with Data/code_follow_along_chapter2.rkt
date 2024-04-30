#lang scheme
; Everything between here and the row of semi-colons is defined for access, and isn't in the order in which it's presented in the book
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))




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
