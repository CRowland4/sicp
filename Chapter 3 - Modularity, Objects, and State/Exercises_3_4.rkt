#lang sicp
; Exercise 3.38 - Time chart drawings in LiquidText
#|
Peter -> Paul -> Mary: Balance = $45
Peter -> Mary -> Paul: Balance $35
Paul -> Peter -> Mary: Balance = $45
Paul -> Mary -> Peter: Balance = %50
Mary -> Peter -> Paul: $40
Mary -> Paul -> Peter: $40
|#



; Exercise 3.39
#|
(define x 10)
(define s (make-serializer))
(parallel-execute
  (lambda () (set! x ((s (lambda () (* x x))))))
  (s (lambda () (set! x (+ x 1)))))
|#
; The first, second, and fifth options would still be possible, with ouputs of 101, 121, and 100.



; Exercise 3.40
#|
(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))
|#
; 1 - P1 sets x to 100 and then P2 sets x to 1,000,000
; 2 - P1 access x once, then P2 sets x to 1,000, then P1 sets x to 10,000
; 3 - P1 access x once, then P2 accesses x once, then P1 sets x to 100, then P2 sets x to 100,000
; 4 - P1 accesses x twice, then P2 accesses x three times, then P1 sets x to 100, and P2 sets x to 1,000
; 5 - P1 accesses x twice, then P2 accesses x three times, then P2 sets x to 1,000, and P1 sets x to 100

#|
(define x 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
|#
; After this serialization, only P1 then P2, or P2 then P1 can be run, both giving a final result of 1,000,000



; Exercise 3.41
#|
The purpose of serializing procedures is so that there aren't any unwanted consequences from interleaving parts of a
  procedure that would access the same account (in this case). So the question we need to ask is, are there any unwanted
  consequences of having the balance procedure interleaved with withdraw or deposit?
The possible situations without serialization of the balance procedure are this:
  We could have withdraw access balance, then the balance procedure access balance, then withdraw set balance, and then
    technically the user that called the balance procedure would be shown a balance greater than the actual balance now that
    withdraw has completed. While not 100% ideal, it's the same thing that would happen if the person that called balance
    had accessed the account balance just a few seconds earlier - balance completes, showing the user a balance, then
    someone withdraws money, and the balance is no longer valid.
  The same thing could happen with deposit, except now the balance would show a smaller amount to the user that accessed
    the balance. But again, this exact same situation could happen without interleaving.
There aren't really any big negative consequences to having the balance procedure un-serialized, but it seems just a bit
  cleaner to me, even if it's not necessary since the balanace procedure doesn't actually change the value of the balance
  variable based on a previously-read value for that variable.
|#



; Exercise 3.42
#|
THIS IS MY INITIAL, INCORRECT ANSWER
The difference here is that the protected procedure is only being called once, at the creation of the account. So the list
  of operations in the serializer only has two procedures in it, withdraw and deposit, in that order. This would prevent withdraw
  and deposit from interleaving with each other, but it wouldn't prevent withdraw calls from interleaving with each other or deposit
  calls from interleaving with each other.

*After checking my answer against other answers online, it seems that I was wrong. I had imagine a serializer as essentially a queue,
  where calling a serialized procedure adds that procedure to the queue. Once the procedure is executed, it's removed from the queue.
  So if we had procedures A1, A2, and A3 in one serializeer and procedures B1, B2, and B3 in another serializer, the As could run
  concurrently with the Bs, but an A couldn't run at the same time as another A, and a B couldn't run at the same time as another B.
  I had pictured the serializer of this problem being the list (withdraw deposit), and erroneously answered as if each call to
  withdraw or deposit from the account would return an *instance of these procedures that aren't in the queue. But that's not what
  happens - THE procedure is actually returned, and the book has this to say about serializers a few pages above:

    "...serialization creates distinguished sets of procedures such that only one execution of a procedure in each serialized set is
     is permitted to happen at a time. If some procedure in the set is being executed, then a process that attempts to execute any
     procedure in the set will be forced to wait until the first execution has finished."

  Even if a two calls to a procedure are being executed concurrently, the procedure being used by both processes is at the same place
    in memory, so the serializer could check if it's being executed. It's not a copy of the procedure somewhere else in memory like I
    originally imagined.
  So ultimately this is a safe change to make. I may have more notes to add here once I actually see the implementation of make-serializer
    in the next section.
|#



; Exercise 3.43
#|
a.) Induction: If after n exchanges the balances in the three accounts are (10, 20, 30) in some order, then after one more exchange, the
      balances will still be (10, 20, 30) in some order.
    With initial balances of (10, 20, 30) after 0 exchanges, doing one exchange leaves you with (10, 20, 30) in the accounts, in some order.
      So by induction, the original hypothesis from the problem is true. It's not as rigorous as a math proof would be but the idea still works.

b.) In LiquidText

c.) We can think of the "total of all accounts" as it's own value, call it T, where T is comproside of the balances of the three accounts:
      A + B + C. Therefore, the operations that affect those three accounts are the only operations that can affect T. But for each of the
      three accounts, the set of operations that can affect that account (the account's withdraw and deposite procedures) are serialized.
      Since each exchange operation subtracts the same amount from one account that it adds to another, and we know that operations on individual
      accounts are reliable, we know that T will remain unchanged.

d.) In LiquidText   
|#



; Exercise 3.44
#|
Ben Bitdiddle is right - as long as the account mechanisms are serialized, this will work fine. The fundamental difference between this and the
  exchange procedure is that the exchange procedure's outcome (the final amount in the account) depeneded on the balance of the account at the
  time of the initial "access" used to calculate the difference. The balance was then read more times within the withdraw and deposit procedures.

In this transfer procedure, the amount being withdrawn and deposited is independant of the current account balance. In other words, the final state
  of an account doesn't depend on it's own current state that must be read separately from/outside of the serialized processes.
|#



; Exercise 3.45
#|
With this method, the serialized procedures, for both accounts, would contain withdraw, deposit, and exchange. But deposit and withdraw are used
  *within* exchange, meaning that they would never execute since the procedure that calls them, also being serialized, would need to finish first.
  But, of course, exchange can't finish until deposite and withrdraw are both called, so everything would just lock up.
|#



; Exercise 3.46 - In LiquidText



; Exercise 3.47
; a.) In terms of mutexes
(define (make-semaphore-mutexes n)
  (let ((mutex (make-mutex))
        (taken 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (< taken n)
                 (begin
                   (set! taken (+ taken 1))
                   (mutex 'release))
                 (begin
                   (mutex 'release)
                   (the-semaphore 'acquire))))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! taken (- taken 1))
             (mutex 'release))))
    the-semaphore))

; b.) In terms of test-and-set! operations
(define (make-semaphore-test-and-set n)
  (let ((cell (list false))
        (taken 0))
    (define (acquire)
      (cond ((test-and-set! cell) (acquire))
            ((< taken n)
             (set! taken (+ taken 1))
             (clear! cell))
            (else
             (clear! cell)
             (acquire))))
    (define (release)
      (cond ((test-and-set! cell) (release))
            ((> taken 0)
             (set! taken (- taken 1))
             (clear! cell))
            (else
             (clear! cell))))
    (define (dispatch m)
      (cond ((= m 'acquire) (acquire))
            ((= m 'release) (release))
            (else (error "Bad semaphore command"))))
    dispatch))



; Exercise 3.48
#|
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))
|#
(define (make-account balance number)  ; Note, this would leave ID management and non-duplication up to the user
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (account-number)
    number)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'account-number) account-number)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (cond (((account1 'account-number) < (account2 'account-number))
           ((serializer1 (serializer2 exchange)) account1 account2))
          (((account2 'account-number) < (account1 'account-number))
           ((serializer2 (serializer1 exchange)) account1 account2)))))

#|
This works because there is a sequence that must be followed. In the example of two exchanges happening that share
  an account, such as a1 -> a2 and a2 -> a1, the second process will not be able to begin until it has the locks for a1
  (assuming that a1 has a smaller account ID than a2). Since the entire exchange process is serialized, this means that
  only one exchange at a time per account can be happening. So in the case of a1 -> a2 and a2 - a1 being called at the same
  time, one of the exchanges would finish completley before the other one begins. This does not however, prevent a set of
  exchanges like a1 -> a2 and a3 -> a4 from happening concurrently.

This is a note to convince myself that (serializer1 (serializer2 exchange)) is in fact the correct order to call the
  serializers, given that account ID 1 is smaller than account ID 2. The site I reference for solution checking and
  help contains solutions that have both orders. While technically it wouldn't matter, since the important thing here is
  that there is some order that must be obeyed, and hence "largest ID first" and "smallest ID first" would accomplish
  the same thing, I nonetheless wanted to make sure my understanding was correct. 


(serialize2 exchange):
  - acquire mutex 2
  - exchange
  - release mutex 2

(serialize1 (serialize2 exchange)): - mutex 1 does indeed get acquired first
  - acquire mutex 1
    - acquire mutex 2
    - exchange
    - release mutex 2
  - release mutex 1
|#



; Exercise 3.49
#|
Consider some variant of an exchange procedure where you enter two account numbers whose funds are to be withdrawn and then
  the combined sum deposited in a third account. But the third account is based on some calculation involving the sum of
  the initial accounts' funds. Suppose we call this procedure with accounts A and B, so that the mutexes are acquired for
  both of these accounts. Then suppose we also call this procedure with accounts C and D, so that both of the locks for
  those accounts are acquired.

If the A/B procedure determines that the account into which the total amount will be deposited is D, it will not be able
  to acquire the lock for account D until the C/D procedure finishes. But if the C/D procedure determines that the third
  account to receive the funds is A, the C/D process won't be able to finish until the A/B process is finsihed, resulting
  in a deadlock.

The deadlock-avoidance mechanism described in the previous problem does not work here, because processes A/B and C/D are two
  different instances of the same process, and therefore the mechanism could not be applied accross all four initial accounts.
|#