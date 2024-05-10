#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (make-product (caddr p) (cadddr p))))

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (make-sum (caddr s) (cdddr s))))

(define (multiplier p)
  (cadr p))

(define (addend s)
  (cadr s))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)  ; One of the factors is 0
        ((=number? m1 1) m2)  ; One of the factors is 1
        ((=number? m2 1) m1)  ; One of the factors is 1
        ((and (number? m1) (number? m2)) (* m1 m2))  ; Both of the factors are numbers that aren't 0 or 1
        (else (list '* m1 m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)  ; One of the items is 0
        ((=number? a2 0) a1)  ; One of the items is 0
        ((and (number? a1) (number? a2))  ; Both items are numbers
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (join joiner items)
  (let ((result (cdr (accumulate
                      (lambda (x y) (append (list joiner x) y))
                      '()
                      items))))
    (if (= (length result) 1)
        (car result)
        result)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch-huffman branch))
        ((= bit 1) (right-branch-huffman branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (left-branch-huffman tree)
  (car tree))
(define (right-branch-huffman tree)
  (cadr tree))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (make-leaf-set pairs)  ; Pairs is a list of (symbol, weight) pairs
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set-huffman (make-leaf (car pair)  ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (adjoin-set-huffman x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set-huffman x (cdr set))))))

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.53
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes bue socks)



; Exercise 2.54
(define (equal?-lists list1 list2)
  (cond ((or (and (not (pair? list1)) (pair? list2))
             (and (not (pair? list2)) (pair? list1)))
         false)
        ((and (not (pair? list1)) (not (pair? list2)) (eq? list1 list2))
         true)
        (else (and (equal?-lists (car list1) (car list2))
                   (equal?-lists (cdr list1) (cdr list2))))))
(equal?-lists '(this is a list) '(this is a list)) ; #t
(equal?-lists '(this is a list) '(this (is a) list)) ; #f



; Exercise 2.55
(car ''abracadabra)
#|
The innter >'abracadabra< is the same as (quote abracadabra).
Then you have '(quote abracadabra), which is a list of symbols, the first of which is 'quote,
   and (car <a list of symbols starting with 'quote>) is going to return 'quote, which is displayed as > quote.
|#



; Exercise 2.56
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var)))
        ((and (exponentiation? expr) (number? (exponent expr)))
         (make-product (exponent expr)
                       (make-product (make-exponentiation (base expr) (- (exponent expr) 1))
                                     (deriv (base expr) var))))
        ((product? expr)
         (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        (else
         (error "unknown expression type: DERIV" exp))))
(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define (exponent expr)
  (caddr expr))
(define (base expr)
  (cadr expr))
(define (make-exponentiation base expn)
  (cond ((and (number? base) (number? expn))
         (expt base expn))
        ((=number? expn 1) base)
        ((=number? expn 0) 1)
        (else (list '** base expn))))



; Exercise 2.57
(define (multiplicand-new p)
  (if (= (length p) 3)
      (caddr p)
      (make-product (caddr p) (cadddr p))))
(define (augend-new s)
  (if (= (length s) 3)
      (caddr s)
      (make-sum (caddr s) (cdddr s))))



; Exercise 2.58
(define (deriv-infix expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum?-infix expr) (make-sum-infix (deriv-infix (addend-infix expr) var)
                               (deriv-infix (augend-infix expr) var)))
        ((and (exponentiation? expr) (number? (exponent expr)))
         (make-product-infix (exponent expr)
                       (make-product-infix (make-exponentiation (base expr) (- (exponent expr) 1))
                                     (deriv-infix (base expr) var))))
        ((product?-infix expr)
         (make-sum-infix
          (make-product-infix (multiplier-infix expr)
                        (deriv-infix (multiplicand-infix expr) var))
          (make-product-infix (deriv-infix (multiplier-infix expr) var)
                        (multiplicand-infix expr))))
        (else
         (error "unknown expression type: DERIV" exp))))
(define (multiplicand-infix p)
  (caddr p))
(define (augend-infix s)
  (caddr s))
(define (multiplier-infix p)
  (car p))
(define (addend-infix s)
  (car s))
(define (make-product-infix m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)  ; One of the factors is 0
        ((=number? m1 1) m2)  ; One of the factors is 1
        ((=number? m2 1) m1)  ; One of the factors is 1
        ((and (number? m1) (number? m2)) (* m1 m2))  ; Both of the factors are numbers that aren't 0 or 1
        (else (list m1 '* m2))))
(define (make-sum-infix a1 a2)
  (cond ((=number? a1 0) a2)  ; One of the items is 0
        ((=number? a2 0) a1)  ; One of the items is 0
        ((and (number? a1) (number? a2))  ; Both items are numbers
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (sum?-infix x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product?-infix x)
  (and (pair? x) (eq? (cadr x) '*)))
(define (exponentiation?-infix expr)
  (and (pair? expr) (eq? (cadr expr) '**)))
(define (exponent-infix expr)
  (caddr expr))
(define (base-infix expr)
  (car expr))
(define (make-exponentiation-infix base expn)
  (cond ((and (number? base) (number? expn))
         (expt base expn))
        ((=number? expn 1) base)
        ((=number? expn 0) 1)
        (else (list base '** expn))))



; PEMDAS deriv procedure
(define (deriv-pemdas expr var)
  (cond ((number? expr) 0)
        ((variable?-pemdas expr) (if (same-variable?-pemdas expr var) 1 0))
        ((sum?-pemdas expr) (make-sum-pemdas (deriv-pemdas (addend-pemdas expr) var)
                               (deriv-pemdas (augend-pemdas expr) var)))
        ((and (exponentiation? expr) (number? (exponent expr)))
         (make-product-pemdas (exponent expr)
                       (make-product-pemdas (make-exponentiation (base expr) (- (exponent expr) 1))
                                     (deriv-pemdas (base expr) var))))
        ((product?-pemdas expr)
         (make-sum-pemdas
          (make-product-pemdas (multiplier-pemdas expr)
                        (deriv-pemdas (multiplicand-pemdas expr) var))
          (make-product-pemdas (deriv-pemdas (multiplier-pemdas expr) var)
                        (multiplicand-pemdas expr))))
        (else
         (error "unknown expression type: DERIV" exp))))
; Predicates
(define (variable?-pemdas x)  ; No change
  (symbol? x))
(define (same-variable?-pemdas v1 v2)  ; No change
  (and (variable?-pemdas v1) (variable? v2) (eq? v1 v2)))
(define (=number?-pemdas expr num)  ; No change
  (and (number? expr) (= expr num)))
(define (sum?-pemdas expr)  ; Changed - now says whether or not the lowest precedent operator in the expression is +
  (eq? (lowest-precedence-op expr) '+))
(define (product?-pemdas expr)  ; Changed - now says whether or not the lowest precedent operator in the expression is *
  (eq? (lowest-precedence-op expr) '*))
(define (exponentiation?-pemdas expr)  ; Changed - now says whether or not the lowest precedent operator in the expression is **
  (eq? (lowest-precedence-op expr) '**))

; Selectors
; New procedure lowest-precedent-op
(define (lowest-precedence-op expr)
  (cond ((not (eq? (memq '+ expr) false)) '+)
        ((not (eq? (memq '* expr) false)) '*)
        ((not (eq? (memq '** expr) false)) '**)
        (else (false))))
; New procedure memq-inverse
(define (memq-inverse item expr)
  (define (iter current-items current-expr)
    (cond ((or (null? current-expr) (= (length current-expr) 1)) false)
          ((eq? (cadr current-expr) item) (append current-items (list (car current-expr))))
          (else (iter (append current-items (append (list (car current-expr) (cadr current-expr))))
                      (cddr current-expr)))))
  (iter '() expr))
(define (multiplier-pemdas p)  ; Chnge multiplier to memq-inverse
  (let ((result (memq-inverse '* p)))
    (if (= (length result) 1)
        (car result)
        result)))
(define (multiplicand-pemdas p)  ; Change multiplicand to memq
  (let ((result (cdr (memq '* p))))
    (if (= (length result) 1)
        (car result)
        result)))
(define (augend-pemdas s)  ; Change augend to memq
  (let ((result (cdr (memq '+ s))))
    (if (= (length result) 1)
        (car result)
        result)))
(define (addend-pemdas s)  ; Chnge addend to memq-inverse
  (let ((result (memq-inverse '+ s)))
    (if (= (length result) 1)
        (car result)
        result)))
; Constructors - None of these get changed
(define (make-product-pemdas m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)  ; One of the factors is 0
        ((=number? m1 1) m2)  ; One of the factors is 1
        ((=number? m2 1) m1)  ; One of the factors is 1
        ((and (number? m1) (number? m2)) (* m1 m2))  ; Both of the factors are numbers that aren't 0 or 1
        (else (list '* m1 m2))))
(define (make-sum-pemdas a1 a2)
  (cond ((=number? a1 0) a2)  ; One of the items is 0
        ((=number? a2 0) a1)  ; One of the items is 0
        ((and (number? a1) (number? a2))  ; Both items are numbers
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (make-exponentiation-pemdas base expn)
  (cond ((and (number? base) (number? expn))
         (expt base expn))
        ((=number? expn 1) base)
        ((=number? expn 0) 1)
        (else (list base '** expn))))



; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))



; Exercise 2.60
#|
This is the same procedure - in each case, the first time the procedure sees the desired element, true is returned.
However, the duplicates sets will take longer to return a false, because there are more elements to iterate through and discard.
So overall, the non-duplicates context is better here.
|#
(define (element-of-set?-duplicates x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set?-duplicates x (cdr set)))))

#|
With a set representation that allows for duplicates, adjoin is much faster because we don't have to first check
   whether or not the element is already in the set.
|#
(define (adjoin-set-duplicates x set)
      (cons x set))

#|
The union procedure is also better in the duplicates version, because again we don't have to check whether or not any
   element is a member of any set - we just append one set to the other.
|#
(define (union-set-duplicates set1 set2)
  (append set1 set2))

#|
The intersection procedure also isn't any better, and will actually take more time given that a duplicate element won't contribute
   anything to the result set other than an extra call to element-of-set?.
|#
(define (intersection-set-duplicates set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set?-duplicates (car set1) set2)
         (cons (car set1) (intersection-set-duplicates (cdr set1) set2)))
        (else (intersection-set-duplicates (cdr set1) set2))))

#|
If there's a circumstance where combining sets or adding elements to a set is the primary operation, this representation could be useful.
But at some point that usefulness runs up agains memory usage. Also, presumably you'll eventually want to run membership tests, otherwise
   why would you want a set to begin with? Overall, when it comes to sets, I'd prefer the non-duplicate representation. It also aligns more
   easily with our intuition, which is important in maintenance terms.
|#



; Exercise 2.61
; Here, we don't need to make any calls to element-of-set?, and the recursive calls will stop, on average, about halfway through the list.
(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x))
        ((= (car set) x) set)
        ((< (car set) x) (cons (car set) (adjoin-set-ordered x (cdr set))))
        ((> (car set) x) (cons x set))))



; Exercise 2.62
(define (union-set-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (union-set-ordered (cdr set1) set2))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set-ordered (cdr set1) set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set-ordered set1 (cdr set2))))))



; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))
; At first glance it looks like they're just the recursive and iterative versions, respectively,
;   of the same list and should always return the same result.
(define tree1 (make-tree 7
                         (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 '() (make-tree 11 '() '()))))
(define tree2 (make-tree 3 (make-tree 1 '() '())
                          (make-tree 7 (make-tree 5 '() '())
                                     (make-tree 9 '() (make-tree 11 '() '())))))
(define tree3 (make-tree 5
                         (make-tree 3 (make-tree 1 '() '()) '())
                         (make-tree 9 (make-tree 7 '() '()) (make-tree 11 '() '()))))
(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)
; First thought was right, these all give the same results

; The second of the two methods (the iterative one) grows more slowly, given the fact that it is both tail recursive,
;   and doesn't ever make any calls to append, instead using cons.



; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))
#|
In the first let, we build the full left branch of the entry point of the main tree.
The next let separates out that left tree from the elements that will go on the right branch of the main tree.
Next, with a recursive call a tree is created from all but the first of the remaining elements from the step above.
Then that right tree is separated from the elements that weren't part of the right tree.
Then <this-entry>, which is the center of the original list (which is why it's the entry of the original list, so the resulting tree is balanced),
   is passed as the entry point to a call to make-tree along with the left and right trees that were created previously.
The cons of that tree to <remaining-elts> adds any left-over elements to the right-most side of the full tree

Drawing of tree in LiquidText.

The order of growth is O(n), since we're splitting the list down by half until we're left with calls addressing
   the individual elements of the original list, which is made up of n elements.
|#



; Exercise 2.65
(define (union-set-binary-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((full-list (union-set-ordered list1 list2)))
      (list->tree full-list))))

(define (intersection-set-binary-tree set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((full-list (intersection-set-ordered list1 list2)))
      (list->tree full-list))))



; Exercise 2.66
(define (make-record-node data-key data)
  (list data-key data '() '()))
(define (key record-node)
  (car record-node))
(define (get-data record-tree)
  (cadr record-tree))
(define (left-data-branch record-tree)
  (caddr record-tree))
(define (right-data-branch record-tree)
  (cadddr record-tree))

(define (lookup-tree-records given-key records-tree)
  (let ((this-key (key records-tree)))
    (cond ((null? records-tree) false)
        ((= this-key given-key)
         (get-data records-tree))
        ((< this-key given-key)
         (lookup-tree-records given-key (left-data-branch records-tree)))
        ((> this-key given-key)
         (lookup-tree-records given-key (right-data-branch records-tree))))))



; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; Tree drawn in LiquidText
; Message (before running) - (A C A B B D A)
(decode sample-message sample-tree)  ; (A D A B B C A) - I swapped D and C in my drawing (it's corrected now)



; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (huffman-iter current-tree encoding)
    (cond ((and (leaf? current-tree) (eq? (symbol-leaf current-tree) symbol))
           encoding)
          ((leaf? current-tree) false)
          ((eq? (memq symbol (symbols current-tree)) false)
           false)
          (else
           (let ((left (huffman-iter (left-branch-huffman current-tree) (append encoding (list 0)))))
             (if (not (eq? left false))
                 left
                 (let ((right (huffman-iter (right-branch-huffman current-tree) (append encoding (list 1)))))
                   (if (not (eq? right false))
                       right
                       (error "Symbol not found in tree"))))))))
  (huffman-iter tree '()))
(encode '(A D A B B C A) sample-tree)  ; (0 1 1 0 0 1 0 1 0 1 1 1 0)



; Exercise 2.69
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set-huffman (make-code-tree (car leaf-set) (cadr leaf-set))
                           (cddr leaf-set)))))
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(generate-huffman-tree (list (list 'C 1) (list 'D 1) (list 'B 2) (list 'A 4))) 



; Exercise 2.70
(define song-tree (generate-huffman-tree (list (list 'NA 16) (list 'YIP 9) (list 'SHA 3) (list 'GET 2) (list 'A 2) (list 'JOB 2) (list 'WAH 1) (list 'BOOM 1))))
(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(encode message song-tree)
(length (encode message song-tree))  ; 84 total bits
; There are 8 total "words", so we would need at least 8 unique combinations of bits, which can be accomplished
;   with a binary string of length three. Then we would need to add up the number of characters and multiply it by 3,
;   which gives (1 + 1 + 2 + 2 + 2 + 3 + 16 + 9) * 3 = 108



; Exercise 2.71
; In general, only one bit is required to encode the most frequent symbol.
; The least frequent symbol will require n - 1 bits.



; Exercise 2.71
#|
For the specific case of exercise 2.70, for the least-frequent symbol, you have to make (n - 1) searches.
Since the number of elements in the list your searching decreases by one each time, the search time averages out to (n - 1)/2 which is O(n) time.
But these are just the searches - you have to do (n - 1) of them each time, so the total order of growth is O(n^2)
For the most-frequent symbol, the order of growth is O(n), since you just have to search once befor eyou find the character.

The most general case would need to take into account not only the number of symbols, but also the distribution of the frequencies of those symbols,
  which would determine the shape of the tree, so I'm not going to spend time calculating that, but I will look it up and try to understand it.
Assuming the tree is balanced with respect to the weights, the general case would be O(nlog(n)) - n foreach the searches that happen at log(n) branches.
That's as far as I care to take this one.
|#
