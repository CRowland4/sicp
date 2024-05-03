#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them
(define nil '())

(define (square x)
  (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

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

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.17
(define (last-pair in-list)
  (let ((but-first (cdr in-list)))
    (if (null? but-first)
        in-list
        (last-pair but-first))))



; Exercise 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))



; Exercise 2.19
; Previous change-counting procedure here for reference
#|
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
|#
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
; The order does not matterbut having the biggest denomination first is more efficient.
; For example, for 100 cents where 50 comes first, we know right from the start that if fifty is used once, the only reasonable numbers to calculate now are 50 cents and lower.
;     If the order was different, and 10 (or any other coin smaller than 50) came first, our process would form a tree that will eventually verify if 50 divides evenly into 90,
;     and then 80, 70, ... and so on. This produces extra steps that were skipped when we removed 50 first, ensuring that we're only going to be iterating through instances where
;     50 can be used to actually make change.
; Example below demonstrates how order doesn't matter

#|  Commenting out so it doesn't slow down the rest of the runs
(define us-coins (list 50 25 10 5 1))
(define random-us-coins (list 10 1 25 50 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define random-uk-coins (list 2 5 10 0.5 100 1 20 50))

(cc 100 us-coins)  ; 292
(cc 100 random-us-coins)  ; 292
(cc 100 (reverse us-coins))  ; 292
(cc 100 (reverse random-us-coins))  ; 292
(cc 100 uk-coins)  ; 104561
(cc 100 random-uk-coins)  ; 104561
(cc 100 (reverse uk-coins))  ; 104561
(cc 100 (reverse random-uk-coins))  ; 104561
|#



; Exercise 2.20
(define (same-parity x . ints)
  (let ((parity (remainder x 2)))
    (define (iter result items)
      (cond ((null? items) result)
            ((= parity (remainder (car items) 2))
             (iter (append result (list (car items))) (cdr items)))
            (else (iter result (cdr items)))))
    (iter '() ints)))



; Exercise 2.21
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))



; Exercise 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))  ; <answer> is initially empty. These two lines are combining the first element if items with the answer in a "right to left" way,
                    answer))))             ;    so the list gets reversed
  (iter items nil))

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer  ; This line is using cons on a list and a number, so it will produce a nest of lists rather than a single list. It should use append instead
                    (square (car things))))))
  (iter items nil))



; Exercise 2.23
(define (for-each-alt proc items)
  (if (null? items)
      true
      ((lambda (x) (proc (car x)) (for-each-alt proc (cdr x))) items)))  ; Lambdas are



; Exercise 2.24 - Done in LiquidText



; Exercise 2.25
(define first (list 1 3 (list 5 7) 9))
(define second (list (list 7)))
(define third (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

#|
(car (cdr (car (cdr (cdr first))))) -> 7
(car (car second)) -> 7
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third)))))))))))) -> 7
|#


; Exercise 2.26
#|
(define list-x (list 1 2 3))
(define list-y (list 4 5 6))
(append list-x list-y) -> (1 2 3 4 5 6)
(cons list-x list-y) -> ((1 2 3) 4 5 6)
(list list-x list-y) -> ((1 2 3) (4 5 6))
|#



; Exercise 2.27
(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car items))))))



; Exercise 2.28
(define (fringe items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (fringe (car items)) (fringe (cdr items))))
        (else (append (list (car items)) (fringe (cdr items))))))



; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch len structure)
  (list len structure))

; Part a - Selectors
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

; Part b
(define (total-weight mobile)
  (let ((left-has-mobile (pair? (branch-structure (left-branch mobile))))
        (right-has-mobile (pair? (branch-structure (right-branch mobile)))))
    (cond ((and left-has-mobile right-has-mobile)
           (+ (total-weight (branch-structure (left-branch mobile)))
              (total-weight (branch-structure (right-branch mobile)))))
          (left-has-mobile
           (+ (total-weight (branch-structure (left-branch mobile)))
              (branch-structure (right-branch mobile))))
          (right-has-mobile
           (+ (total-weight (branch-structure (right-branch mobile)))
              (branch-structure (left-branch mobile))))
          (else
           (+ (branch-structure (left-branch mobile))
              (branch-structure (right-branch mobile)))))))

(define test-mobile (make-mobile (make-branch 1 (make-mobile (make-branch 3 4)
                                                             (make-branch 4 (make-mobile (make-branch 5 3) (make-branch 6 7)))))
                                 (make-branch 2 (make-mobile (make-branch 10 (make-mobile (make-branch 7 1) (make-branch 8 2)))
                                                             (make-branch 9 5)))))
(total-weight test-mobile)

; Part c
(define (balanced? mobile)
  (let ((left-structure (branch-structure (left-branch mobile)))
        (right-structure (branch-structure (right-branch mobile)))
        (branches-torques-equal? (= (branch-torque (left-branch mobile))
                                    (branch-torque (right-branch mobile)))))
    (cond ((not branches-torques-equal?) false)
          ((and (pair? right-structure) (pair? left-structure))
           (and (balanced? right-structure) (balanced? left-structure)))
          ((pair? left-structure)
           (balanced? left-structure))
          ((pair? right-structure)
           (balanced? right-structure))
          (else true))))
(define (branch-torque branch)
  (let ((branch-item (branch-structure branch)))
    (if (pair? branch-item)
        (* (branch-length branch) (total-weight branch-item))
        (* (branch-length branch) branch-item))))

(branch-torque (left-branch test-mobile))
(branch-torque (right-branch test-mobile))
(balanced? test-mobile)

; Part d
(define (make-mobile-cons left right)
  (cons left right))
(define (make-branch-cons len structure)
  (cons len structure))
; left-branch can stay the same
(define (right-branch-cons mobile)
  (cdr mobile))  ; This line of right-branch changes from "(cadr mobile))" to "(cdr mobile))"
; branch-length can stay the same
(define (branch-structure-cons branch)
  (cdr branch))  ; This line of branch-structure changes from "(cadr branch))" to "(cdr branch))"
(define (total-weight-cons mobile)  ; Changed procedure references to use the "-cons" versions, but nothing else in total-weight
  (let ((left-has-mobile (pair? (branch-structure-cons (left-branch mobile))))
        (right-has-mobile (pair? (branch-structure-cons (right-branch-cons mobile)))))
    (cond ((and left-has-mobile right-has-mobile)
           (+ (total-weight-cons (branch-structure-cons (left-branch mobile)))
              (total-weight-cons (branch-structure-cons (right-branch-cons mobile)))))
          (left-has-mobile
           (+ (total-weight-cons (branch-structure-cons (left-branch mobile)))
              (branch-structure-cons (right-branch-cons mobile))))
          (right-has-mobile
           (+ (total-weight-cons (branch-structure-cons (right-branch-cons mobile)))
              (branch-structure-cons (left-branch mobile))))
          (else
           (+ (branch-structure-cons (left-branch mobile))
              (branch-structure-cons (right-branch-cons mobile)))))))
(define (balanced?-cons mobile)  ; Changed procedure references to use the "-cons" versions, but nothing else in balanced?
  (let ((left-structure (branch-structure-cons (left-branch mobile)))
        (right-structure (branch-structure-cons (right-branch-cons mobile)))
        (branches-torques-equal? (= (branch-torque-cons (left-branch mobile))
                                    (branch-torque-cons (right-branch-cons mobile)))))
    (cond ((not branches-torques-equal?) false)
          ((and (pair? right-structure) (pair? left-structure))
           (and (balanced? right-structure) (balanced? left-structure)))
          ((pair? left-structure)
           (balanced? left-structure))
          ((pair? right-structure)
           (balanced? right-structure))
          (else true))))
(define (branch-torque-cons branch)  ; Changed procedure references to use the "-cons" versions, but nothing else in branch-torque
  (let ((branch-item (branch-structure-cons branch)))
    (if (pair? branch-item)
        (* (branch-length branch) (total-weight-cons branch-item))
        (* (branch-length branch) branch-item))))
; The only changes necessary were in right-branch and branch-structure, changing cadr to cdr
(define test-mobile-cons (make-mobile-cons (make-branch-cons 1 (make-mobile-cons (make-branch-cons 3 4)
                                                                                 (make-branch-cons 4 (make-mobile-cons (make-branch-cons 5 3) (make-branch-cons 6 7)))))
                                           (make-branch-cons 2 (make-mobile-cons (make-branch-cons 10 (make-mobile-cons (make-branch-cons 7 1) (make-branch-cons 8 2)))
                                                                                 (make-branch-cons 9 5)))))
(branch-torque-cons (left-branch test-mobile-cons))
(branch-torque-cons (right-branch-cons test-mobile-cons))
(balanced?-cons test-mobile-cons)



; Exercise 2.30
(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-direct (car tree))
                    (square-tree-direct (cdr tree))))))
(define (square-tree-recurs tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-recurs sub-tree)
             (square sub-tree)))
       tree))
(define test-tree (list 1
                        (list 2 (list 3 4) 5)
                        (list 6 7)))
(square-tree-recurs test-tree)
(square-tree-direct test-tree)



; Exercise 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(tree-map square test-tree)



; Exercise 2.32
; With the recursion process, we eventually reach a point where the empty set is returned, so <rest> is (()) because of calling list on nil, and a <s> of (3),
;   or the last element of the initial set <s>. So our <rest> becomes (() (3)) in the previous frame. We need to keep these legitimate subsets of the initial <s>,
;   but we also need to add the rest of the numbers from the original set. So we create a new list by adding one more number from <s> into each of the existing sets,
;   and then append this newly created list to the subsets we already have.
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (set) (cons (car s) set))
                      rest)))))
(subsets (list 1 2 3))



; Exercise 2.33
; Main takeaway here is that <x> in the lambda (x y) procedures can be thought of as the next element in the sequence, and y is the result so far
(define (map2.33 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
(map2.33 square (list 1 2 3 4 5))

(define (append2.33 seq1 seq2)
  (accumulate cons seq2 seq1))
(append2.33 (list 1 2 3) (list 4 5 6))

(define (length2.33 sequence)
  (accumulate (lambda (x y) (+ y (if (not (pair? x)) 1 0))) 0 sequence))
(length2.33 (list 1 2 3 4 5 6))



; Exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))



; Exercise 2.35
(define (count-leaves-acc tree)
  (accumulate +
              0
              (map (lambda (x) 1) (enumerate-tree tree))))
(count-leaves-acc test-tree)



; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(define test-seqs (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 test-seqs)



; Exercise 2.37
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define square-matrix (list (list 3 2 2) (list 2 2 2) (list 2 2 2)))
(define vector (list 1 1 1 1))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)  ; Multiplying a matrix by a vector is just taking the dot product of all the matrix rows with the vector
  (map (lambda (row-m) (dot-product row-m v)) m))
(matrix-*-vector matrix vector)

(define (transpose mat)  ; The transpose of a matrix is just a list of the columns
  (accumulate-n cons '() mat))
(transpose matrix)

(define (matrix-*-matrix m n)  ; Matrix multiplication is just multiplying each row vector of the left matrix by transpose of the right matrix
  (let ((cols (transpose n)))
    (map (lambda (row-m) (matrix-*-vector cols row-m)) m)))
(matrix-*-matrix square-matrix square-matrix)



; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
; I'm using "accumulate" instead of "fold-right" so the code can be run, given that accumulate is defined at the top
(accumulate / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(accumulate list nil (list 1 2 3)) ; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ; (((() 1) 2) 3)
; In order for fold-left and fold-right to be equal, op should be associative. In other words (op a (op b c)) should equal (op (op a b) c))



; Exercise 2.39
(define (reverse-fold-right sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))
(reverse-fold-right (list 1 2 3))

; In the lambda for fold-left,  y is the next element, and x is the result so far, the reverse of accumulate (fold-right)
(define (reverse-fold-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse-fold-left (list 1 2 3))



; Exercise 2.40
(define (prime-sum-pairs-simplified n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(prime-sum-pairs-simplified 5)



; Exercise 2.41
(define (summed-triples n s)
  (filter (lambda (triple) (= (accumulate + 0 triple) s)) (distinct-triples n)))
(define (distinct-triples n)
  (flatmap (lambda(i)
             (flatmap (lambda (j)
                    (map (lambda (k) (list i j k))
                         (remove i (remove j (enumerate-interval 1 n)))))
                  (remove i (enumerate-interval 1 n))))
           (enumerate-interval 1 n)))
(distinct-triples 4)
(summed-triples 4 8)