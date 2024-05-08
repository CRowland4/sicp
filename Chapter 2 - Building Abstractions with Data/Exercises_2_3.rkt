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