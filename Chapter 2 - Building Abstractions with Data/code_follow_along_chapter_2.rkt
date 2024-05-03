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
 
            


