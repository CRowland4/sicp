#lang sicp

; Everything between here and the row of semi-colons is defined so that the problems have access to them
(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

; The operations of the queue
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue)
  (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)'
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z  ; (a b c d)
(cdr x)  ; response 1 - (b)
(define w (append! x y))
w  ; (a b c d)
(cdr x)  ; response 2 - (b c d)

; Box and pointer diagrams in LiquidText



; Exercise 3.13
; (last-pair z), where z is a cycle, will cause an infiniute loop since the break condition
;   of (cdr z) never being null will never be reached.



; Exercise 3.14 - Box and Pointer diagrams in LiquidText
; The mystery procedure is a reverse procedre that reverses a list "in place".
; So w is the reverse of v, and v is the a single-element list whose only element
;   is the first element of the original v.



; Exercise 3.15 in LiquidText



; Exercise 3.16 in LiquidText



; Exercise 3.17
(define (count-pairs x)
  (define (iter pair counted)
    (cond ((or (memq pair counted) (not (pair? pair))) 0)
          (else
           (begin
             (set! counted (if (null? counted)
                               (list pair)
                               (append! counted (list pair))))
             (let ((car-count (iter (car pair) counted)))
               (+ car-count
                  (iter (cdr pair) counted)
                  1))))))
  (iter x '()))



; Exercise 3.18
(define (contains-cycle? items)
  (define (iter pair visited)
    (cond ((null? pair) false)
          ((not (pair? pair)) false)
          ((memq pair visited) true)
          (else
           (begin
             (set! visited (cons pair visited))
             (iter (cdr pair) visited)))))
  (iter items '()))



; Exercise 3.19
(define (constant-contains-cycle? items)
  (define (iter first second)
    (if (eq? first second)
        true
        (if (or (null? (cdr second))
                (null? (cddr second)))
            false
            (iter (cdr first) (cddr second)))))
  (if (or (null? (cdr items))
          (null? (cddr items)))
      false
      (iter (cdr items) (cddr items))))



; Exercise 3.20 - in LiquidText



; Exercise 3.21 - in LiquidText
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

#|
Eva is talking about the fact that, to the interpreter, a "queue" looks just like a list where the first item is a list of all items in
   the queue, and the last item is a list with a single term, with that term being the last item in the queue.

To print the queue like we'd expect, we just need to see what the front pointer is looking at, from the perspective of the interpreter.
|#

(define (print-queue queue)
  (front-ptr queue))



; Exercise 3.22
(define (make-procedure-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else (set! front-ptr (cdr front-ptr))
                  front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               front-ptr))))
    (define (print-queue)
      front-ptr)
    (define (dispatch m . args)
      (cond ((eq? m 'empty-queue?)
             (empty-queue?))
            ((eq? m 'delete-queue!)
             (delete-queue!))
            ((eq? m 'insert-queue!)
             (insert-queue! (car args)))
            ((eq? m 'print-queue)
             (print-queue))))
    dispatch))
(define (empty-proc-queue? queue)
  (queue 'empty-queue?))
(define (delete-proc-queue! queue)
  (queue 'delete-queue!))
(define (insert-proc-queue! queue item)
  (queue 'insert-queue! item))
(define (print-proc-queue queue)
  (queue 'print-queue))



; Exercise 3.23
; Implementation of a doubly-linked node
(define (make-dll-node value)
  (list '() '() value))  ; next, prev, value
(define (next dll-node)
  (car dll-node))
(define (prev dll-node)
  (cadr dll-node))
(define (val dll-node)
  (caddr dll-node))
(define (set-next! dll-node item)
  (set-car! dll-node item))
(define (set-prev! dll-node item)
  (set-car! (cdr dll-node) item))
(define (set-val! dll-node item)
  (set-car! (cadr dll-node) item))
; The deque, which is a doubly-linked list so that all operations can be done in constant time
(define (make-dq)
  (cons '() '()))
(define (print-dq dq)
  (define (iter current-list current-node)
    (if (null? current-node)
        current-list
        (iter (append current-list (list (val current-node))) (next current-node))))
  (iter ' () (dq-front-ptr dq)))
(define (set-dq-front-ptr! dq node)
  (set-car! dq node))
(define (set-dq-rear-ptr! dq node)
  (set-cdr! dq node))
(define (dq-front-ptr dq)
  (car dq))
(define (dq-rear-ptr dq)
  (cdr dq))
(define (empty-dq? dq)
  (and (null? (dq-front-ptr dq))
       (null? (dq-rear-ptr dq))))
(define (rear-insert-dq! dq value)
  (let ((new-node (make-dll-node value)))
    (cond ((empty-dq? dq)
           (set-dq-front-ptr! dq new-node)
           (set-dq-rear-ptr! dq new-node)
           (print-dq dq))
          (else
           (set-next! new-node (dq-front-ptr dq))
           (set-prev! (dq-front-ptr dq) new-node)
           (set-dq-front-ptr! dq new-node)
           (print-dq dq)))))
(define (front-insert-dq! dq item)
  (let ((new-node (make-dll-node item)))
    (cond ((empty-dq? dq)
           (set-dq-front-ptr! dq new-node)
           (set-dq-rear-ptr! dq new-node)
           (print-dq dq))
          (else
           (set-next! (dq-rear-ptr dq) new-node)
           (set-prev! new-node (dq-rear-ptr dq))
           (set-dq-rear-ptr! dq new-node)
           (print-dq dq)))))
(define (rear-delete-dq! dq)
  (cond ((empty-dq? dq)
         (error "DELETE! called with an empty dequeue" dq))
        ((eq? (dq-front-ptr dq) (dq-rear-ptr dq))
         (set-dq-front-ptr! dq '())
         (set-dq-rear-ptr! dq '())
         (print-dq dq))
        (else
         (set-prev! (next (dq-front-ptr dq)) '())
         (set-dq-front-ptr! dq (next (dq-front-ptr dq)))
         (print-dq dq))))5
(define (front-delete-dq! dq)
  (cond ((empty-dq? dq)
         (error "DELETE! called with an empty dq" dq))
        ((eq? (dq-front-ptr dq) (dq-rear-ptr dq))
         (set-dq-front-ptr! dq '())
         (set-dq-rear-ptr! dq '())
         (print-dq dq))
        (else
         (set-next! (prev (dq-rear-ptr dq)) '())
         (set-dq-rear-ptr! dq (prev (dq-rear-ptr dq)))
         (print-dq dq))))
