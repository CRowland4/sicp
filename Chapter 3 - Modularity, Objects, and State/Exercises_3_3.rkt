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



; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value))
                        (cdr local-table)))))
      'ok)
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))



; Exercise 3.25
(define (make-table-generalized same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter table keys)
        (let ((record
               (assoc (car keys) (cdr table))))
          (cond ((not record) false)  ; Key doesn't have an associated record
                ((null? (cdr keys))  ; Last key was used, record was found
                 (cdr record))
                (else  ; Record was found, but there are more keys, so we make sure the record is a subtable and continue
                 (if (not (pair? (cdr record)))
                     false  ; We have more keys, but our record is actually a record, not a subtable
                     (iter record (cdr keys)))))))  ; The record is a subtable, so we continue the search on this table with the rest of the keys
      (iter local-table keys))
    (define (insert! value keys)
      (define (iter table keys)
        (if (= (length keys) 1)  ; If we're on the last key,
            (let ((record  ; Try to find the record associated with that key
                   (assoc (car keys) (cdr table))))
              (if record ; If a record exists for that key,
                  (set-cdr! record value)  ; Changes the value of that record to the new value
                  (set-cdr! table  ; Otherwise, create a new record with that value
                            (cons (cons (car keys) value)
                                  (cdr table)))))
            (let ((subtable  ; If we have more keys to go,
                   (assoc (car keys) (cdr table))))  ; Try to find a subtable associated with that key
              (if subtable  ; If there is a record associated with that key,
                  (if (not (pair? (cdr subtable)))  ; And that record holds a value instead of a subtable,
                      (error "Value already exists: " (cdr subtable) " Unable to finish insertion: " keys) ; Throw an error
                      (iter subtable (cdr keys)))  ; But if that record is actually a subtable, continue with the insertion
                  (begin  ; If there isn't a record associated with that key, create a new table there and continue with the insertion
                    (set-cdr! table
                              (cons (cons (car keys) '())
                                    (cdr table)))
                    (iter (cadr table) (cdr keys)))))))
      (iter local-table keys)
      'ok)
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records))
             (car records))
            (else (assoc key (cdr records)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define test (make-table-generalized eq?))
((test 'lookup-proc) '(one two three))
((test 'insert-proc!) 'foo '(one))
((test 'lookup-proc) '(one))
((test 'insert-proc!) 'bar '(four two three))
((test 'lookup-proc) '(four two three))
((test 'insert-proc!) 'again '(four two five))
((test 'lookup-proc) '(four two five))
((test 'lookup-proc) '(four two three))



; Exercise 3.26
#|
For this type of table, we would do away with the "backbone" idea and essentially implement the same structure as
  Exercise 2.66 - each node is a list of four values: (key, value, nodes "less than" key, nodes "greater than" key).
  In this structure, the value of a node could be null, meaning that that node is the head of a "subtable".
|#



; Exercise 3.27 - environment diagrams in LiquidText
#|
The procedure memo-fib computes the nth Fibonacci number in a number of steps proportional to n because,
   after the first time memo-fib is called on a number, there are only 2 operations to be done for each m < n:
    lookup n - 1 and lookup n - 2. This prevents the giant recursion that can normally happen with these types of procedures.
No - memo-fib recursively calles memo-fib, which means that the memoized version of itself is used to find the solutions.
Defining memo-fib to just be (memoize fib) tould mean that memo-fib recursively called fib, which does not contain any calls
   to the lookup table. This version of memo-fib would use the lookup table once for the inital value, then use calls to vanilla
   fib to calculate the result. That one result would be stored in the table for next time, but it wouldn't be accessible when needed
   for other calculations.
|#



; Exercise 3.28
(define (or-gate w1 w2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal w1) (get-signal w2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! w1 or-action-procedure)
  (add-action! w2 or-action-procedure)
  'ok)
(define (logical-or x y)
  (if (or (= x 1) (= y 1))
      1
      0))



; Exercise 3.29
(define (or-gate-circuit in1 in2 out1)
  (let ((w1 (make-wire))
        (w2 (make-wire))
        (w3 (make-wire)))
    (inverter in1 w1)
    (inverter in2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 out1))
  'ok)

; Total delay is the sum of 1 and-gate delay and three inverter-delays
; In a real circuit, the first two inverters could run at the same time (see drawing in LiquidText),
;   but in this program technically they're being called sequentially, and we haven't introduced concurrency yet.



; Exercise 3.30
(define (ripple-carry-adder A-wires B-wires S-wires C)
  (define (iter An Bn Sn C-in)
    (let ((C-out (make-wire)))
      (if Sn
          (begin
            (full-adder (car A) (car Bn) C-in (car Sn) C-out)
            (iter (cdr An) (cdr Bn) (cdr Sn) C-out))
          'ok)))
  (iter A-wires B-wires S-wires C))

#|
Each half-adder has one inverter (I), one or-gate (O), and two and-gates (A). We'll get the delay from the code-circuit itself,
   not the real circuit that the code represents, and assume no concurrency.
One half adder is I + O + 2A, and one full-adder is two half-adders with an or, so 2(I + O + 2A) + O.
Then a ripple-carry-adder with n full-adders is n(2(I + O + 2A) + O), which simplifies to n(2I + 3O + 4A).
|#



; Exercise 3.31
#| This adds the probe action to the <sum> wire's action procedures. Without running the procedure when it's added, nothing
is printed out. While not mandatory, it is nice to see the starting point of a wire, and without (proc) we don't have that
explicit reference.
|#
(probe 'sum sum)

#|Same as above - adds the probe action to the <carry> wire's action procedures, and nothing is printed out.|#
(probe 'carry carry)

#|First, half-adder calls or-gate with wires input-1, input-2, and a wire internal to or-gate.
     The or-gate procedure adds the or-action-procedure to the wires input-1 and input-2, so that whenever either one of the values
        held by those wires changes, the value of the output wire (internal to the or-gate procedure of the half-adder circuit)
        will be updated accordingly.
Then and-gate is called with wires input-1, input-2, and carry.
     The and-gate procedure adds the and-action-procedure to the wires input-1 and input-2, so that whenever either one of the values
        held by those wires changes, the value of the carry wire will be updated accordingly.
Then inverter is called with the carry wire, and a wire internal to the half-adder circuit.
     The inverter procedure adds the invert-input action to the carry wire, so that whenever the value of the carry wire changes,
        the value of the output wire (internal to the half-adder circuit) is updated accordingly.
Finally, the and-gate is called with two wires (d and e) internal to the half-adder circuit, and the sum wire as output.
     The and-action procedure is added to d and e, so that whenever one of their values changes, the value on the sum wire is updated accordingly.
"ok" is printed to the screen.
|#
(half-adder input-1 input-2 sum carry)

#|The signal for the input-1 wire is set to 1, and the action procedures on the input-1 wire are called, since the value changed from 0 to 1.
     The first procedure to run is the or-action-procedure. It is added to the agenda to, after an or-gate-delay of 5, change the value of
        the d wire to 1.
     The last procedure to run is the and-action-procedure. It is added to the agenda to, after an and-gate-delay of 3 (at 8 total), update the value of
        carry to 0, which doesn't actually change the value since it was already 0.
"done" is printed to the screen.|#
(set-signal! input-1 1)

#|Items in the agenda are executed.
The first item in the agenda is removed and executed.
     The value of the d wire internal to half-adder is changed from 0 to 1.
        The d wire has one action procedure attached to it, which is the and-action procedure. It is added to the agenda to, after an and-delay
          of 3 (total time 11), update the value of sum to 0, which doesn't actually change the value since it was already 0.
The next item in the agenda is removed and executed.
     The value of the carry wire is "updated" from 0 to 0, and since there is no change, nothing happens. "done" is printed to the screen.
The last item in the agenda is removed and executed.
     The value of the sum wire is "updated" from 0 to 0, and nothing happens since nothing changed, but also the sum wire has no attached actions.

At this point, sum should be 1, not 0, and the culprit is the missing (proc) in the add-action! procedure on a wire. When wiring together a circuit,
   we initialize the wires we start with - in this case input-1, input-2, sum, and carry. When a wire is created, it's value is automatically set to 0.
   But when we create the half-adder circuit, it also creates two new wires to serve as output wires for the function boxes internal to the half-adder
   circuit. One of those output wires, e, is meant to be the output wire of the inverter box that has carry (value of 0) as it's input. So the initial
   state of e should be 1, but it's not - it's 0 in this case. This means that when the signal passes through the final and-gate of the half-adder circuit
   that contains d and e, the sum comes out incorrectly to 0, since only d has been set to 1 (by the first item in the agenda).
Running each action-procedure one time when it's added serves to propogate the initialization state of the initial wires to the rest of the wires
   in the circuit. Without it, we would have to manually set each wire internal to the circuit by hand before running any sort of simulation.
|#
(propagate)



; Exercise 3.32
#|
When an and gate is initialized with two wires whose values are 0 and 1 respectively, the output wire's value is set to 0.
When we change the 0 value to a 1, the action to run the logical-and procedure is added to the procedure queue.
When we change the 1 value to a 0, the action is added again.

With a queue, actions are executed in accordance with the model that they are simulating. In this model, the 0 wire was set to 1, and the
   1 wire was set to 0. If we used a stack, the actual order of operations in our simulation would be reversed.  In this particular example,
   after the first wire of the and-gate is switched from 0 to 1, we would expect a 1 to propagate through the output wire, because at this
   point both of the and-gate inputs are set to 1. Then when the second wire was switched from 1 to 0, we would expect the output wire's
   value to go back to 0, propagating that through the system. A stack would never propagate a new 1 and 0, because the and-gate values
   would go from 1 0, to 0 0, to 0 1, meaning the value of the output wire would never change.
|#



; Exercise 3.33
; Constraint: c = (a + b) / 2 --> 2c = a + b - constraint diagram in LiquidText
(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (multiplier v c u)
    (adder (a b u))
    (constant 2 v)
     'ok))



; Exercise 3.34
#|
A multiplier should take 3 connectors - here one connector is being passed twice as if it were two separate connectors.
If the value of the a connector is ever forgotten, then the multiplier will have two empty slots (m1 and m2) and will not
  be able to calculate the third value appropriately.
|#