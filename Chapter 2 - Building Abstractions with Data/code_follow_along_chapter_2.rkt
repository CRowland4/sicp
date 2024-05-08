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

(define (square x)
  (* x x))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

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

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (add-vect v w)
  (cons (+ (xcor-vect v) (xcor-vect w))
        (+ (ycor-vect v) (ycor-vect w))))

(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define (scale-vect scalar v)
  (cons (* scalar (xcor-vect v))
        (* scalar (ycor-vect v))))

(define (draw-line frame segment)
  ("a procedure for drawing the given line segment in the give frame"))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (sub-vect v w)
  (cons (- (xcor-vect v) (xcor-vect w))
        (- (ycor-vect v) (ycor-vect w))))

(define (make-vect x y)
  (cons x y))

(define (key record-node)
  (car record-node))

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



; Procedure for appending one list to another
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



; Procedure for scaling a tree, analagous to scale-list
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(define test-tree (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))
(scale-tree test-tree 10)

; Same procedure, but using map
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
(scale-tree-map test-tree 10)



; Procedure for computing the sum of squares of odd leaves of a tree
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

; Make a list of all even Fibonacci numbers Fib(k) where k is less than or equal to a given integer n
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
    (next 0))



; Procedure for filtering (this is actually a primitive in Scheme)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(filter odd? (list 1 2 3 4 5))



; Procedure for accumulation
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))



; Procedure for enumerating integers over an interval
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(enumerate-interval 2 7)



; Procedure for enumerating the leaves of a tree
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(enumerate-tree (list 1 (list 2 (list 3 4) 5)))



; New way to sum the squares of the odd leaves of a tree using these new procedures
(define (sum-odd-squares-new tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

; Similar rewrite of the gathering of the even Fibonacci numbers below a threshold
(define (even-fibs-new n)
  (accumulate
   cons nil (map fib (enumerate-interval 0 n))))

; Make a list of squares of the first n + 1 Fibonacci numbers
(define (list-fib-squares n)
  (accumulate
   cons nil (map square (map fib (enumerate-interval 0 n)))))
(list-fib-squares 10)

; Prodcut of the squares of odd integers in a sequence
(define (product-of-squares-of-odd-elements sequence)
  (accumulate
   * 1 (map square (filter odd? sequence))))
(product-of-squares-of-odd-elements (list 1 2 3 4 5))

; Potential procedure to find highest-paid programmer salary from a sequence of employee records
#|     Commented out because this is just a hypothetical, so we don't have the procedures used here
(define (salary-of-highest-paid-programmer records)
  (accumulate max 0 (map salary (filter programmer? records))))
|#



; Procedure to find all ordered pairs (i, j) where 1 <= j < i <= n
(define (pairs n)
  (accumulate
   append nil (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
(pairs 5)

; Generalization of the above procedure into a procedure that maps and accumulates
; flatmap takes a procedure and a sequence that would normally generate a list of nested lists,
;   and returns a list of the lists inside the nested lists
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; Predicate procedure for filtering out the pairs from a list of pairs whose sum isn't prime
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

; Procedure that makes a triplet from a pair, where the third item is the sum of the elements of the pair
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; Complete procedure to find all ordered pairs (i, j) where 1 <= j < i <= n and i + j is prime
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                             (enumerate-interval 1 n)))))
(prime-sum-pairs 5)



; Procedure for finding all the permutations of a sequence
(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(permutations (list 1 2 3))



; How the remove primitive can be implemented
(define (remove-example item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))
(remove-example 1 (list 1 2 3))



; The picture language - this is all theoretical and won't actually produce pictures in DrRacket, but I'm writing them out for better understanding
(define painter (lambda (frame) frame)) ; A painter takes a frame and draws it's picture (implemented in <segments->painter> below) inside that frame
(define wave (painter "a waving guy"))
(define (beside painter1 painter2)  ; Painter that draws <painter1> on the left and <painter2> on the right
  painter)
(define (below painter1 painter2)  ; Painter that draws <painter1> on the bottom half and <painter2> on the top half
  painter)
(define (flip-vert painter)  ; Painter that draws <painter> upside down
  painter)
(define (flip-horiz painter)  ; Painter that draws the the image of <painter> left-to-right reversed
  painter)
(define (rotate180 painter)  ; Painter that draws the image of painter rotated 180 degrees
  painter)

; Combining painters
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

; Abstracted procedure that generated wave4
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(define wave4-abstracted (flipped-pairs wave))

; Recursive patterns
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

; Abstracting painter operations
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs-abstraction painter)
  (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
    (combine4 painter)))

(define (square-limit-abstraction painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))



; Frames
(define frame (list "origin" "edge1" "edge2"))

; Constructor
(define (make-frame origin-vector edge-vector1 edge-vector2)
  (list frame origin-vector edge-vector1 edge-vector2))

; Selectors
(define (origin-frame frame)
  (list-ref frame 0))
(define (edge1-frame frame)
  (list-ref frame 1))
(define (edge2-frame frame)
  (list-ref frame 2))

; Frame coordinate map
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))



; Creating a painter from line drawings, with an assumed procedure draw-line.
(define (segments->painter segment-list)  ; Returns a painter, that takes a frame as its single argument
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))



; Procedure to transform a painter
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

; Procedure flip-vert defined with transform-painter
(define (flip-vert-general painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 1.0 1.0) ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

(define (shrink-to-upper-right painter)
  (transform-painter
   painter
   (make-vect 0.5 0.5)
   (make-vect 1.0 0.5)
   (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))



; Procedure for <beside> in terms of painter transformations
(define (beside-transform painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


; Normal example of language expression via a factorial procedure
(define (fact n)
  (if (= n 1) 1 (* n (fact (- n 1)))))
(fact 5)



; Examples of the single quote
(define a 1)

(define b 2)

(list a b) ; > (1 2)

(list 'a 'b) ; > (a b)

(list 'a b) ; (a 2)

(list 'car (list 'quote '(a b c))) ; (car '(a b c))

(car '(a b c)) ; a

(cdr '(a b c)) ; (b c)



; Implementation of memq primitive
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '( x (apple sauce) y apple pear))



; Symbolic differentiation of sums and products
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var)))
        ((product? expr)
         (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        (else
         (error "unknown expression type: DERIV" exp))))
(define (variable? x)
  (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum-old a1 a2) ; This version of make-sum doesn't reduce sums to their lowest forms. Redefined below.
  (list '+ a1 a2))
(define (make-product-old m1 m2) ; This version of make-product doesn't reduce products to their lowest forms. Redefined below.
  (list '* m1 m2))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s)
  (cadr s))
(define (augend s)
  (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p)
  (cadr p))
(define (multiplicand p)
  (caddr p))

#|
None of the solutions here are reduced to lowest form withouot the new make-sum defined below

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
|#



; Redefine <make-sum> so it reduces sums to their lowest form
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (=number? expr num)
  (and (number? expr) (= expr num)))

#|
The sums are reduced, but we still need to reduce the products
(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
|#

; Redefine <make-product> so it reduces products to their lowest forms
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)



; Implementation of sets as unordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))



; Implementation of sets as ordered lists
(define (element-of-set?-ordered x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ordered (cdr set1) (cdr set2))))
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ((< x2 x2)
               (intersection-set-ordered (set1 (cdr set2))))))))



; Implementation of sets as binary trees
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set?-binary-tree x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set?-binary-tree x (left-branch set)))
        ((> x (entry set))
         (element-of-set?-binary-tree x (right-branch set)))))

(define (adjoin-set-binary-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-binary-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-binary-tree x (right-branch set))))))



; Database lookup for a set of records implemented as an unordered list
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))