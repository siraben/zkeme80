\ A collection of little fun backtracking experiments.
." Backtracking" CR

\ This example uses the exception system (CATCH and THROW) to
\ implement backtracking.
\ Source:
\ https://web.archive.org/web/20190107141051/https://www.complang.tuwien.ac.at/forth/backtracking-in-ansforth

: PUSH-RETURN ( ret-stack1 ret-addr -- ret-stack2 )
  HERE 2 CELLS ALLOT
  SWAP OVER CELL+ !
  SWAP OVER !
;

: POP-RETURN ( ret-stack1 -- ret-stack2 ret-addr )
  DUP CELL+ @ 
  SWAP @
;

: CHOICE
  \ works like RETURN ( ret-stack1 -- ret-stack2 ), but creates a
  \ choicepoint i.e., it catches a failure; exits upon failure with
  \ stack effect ( ret-stack -- )
  POP-RETURN CATCH
  DUP 1 <> IF \ it's not a failure, but something else
    THROW
  THEN
  DROP
;


: RETURN ( ret-stack1 -- ret-stack2 ) 
  \ never exits; stack-effect with respect to the word returned to.
  POP-RETURN EXECUTE
;


: FAIL \ neither exits nor returns
  1 THROW ;

: BEGIN-CHOICES ' >R , ; IMMEDIATE
: END-CHOICES  ' R> , ' RETURN , ; IMMEDIATE
: MAYBE ' R@ , ' CHOICE , ; IMMEDIATE

: ONE-TO-FOUR ( RET-STACK1 -- N RET-STACK2 )
  BEGIN-CHOICES
  1 MAYBE 2 MAYBE 3 MAYBE 4
  END-CHOICES
;

: FOO-PART2 ( ret-stack -- ... )
  \ never exits, never returns we can treat "." as primitive here; it
  \ does not do choicepoints, failure etc.
  >R . FAIL
;

: FOO ( ret-stack -- ... ) \ never exits, never returns
  ' FOO-PART2 SWAP PUSH-RETURN ONE-TO-FOUR
;

: SUCCEED
  \ get out of the return/backtracking mode
  2 THROW
;

: WRAPPER ( xt -- )
  \ execute xt in return/backtracking mode
  ' SUCCEED 0 PUSH-RETURN SWAP CATCH
  CASE
    1 OF ." FAILURE" ENDOF
    2 OF ." SUCCESS" ENDOF
    THROW
  ENDCASE
  DROP
;

' FOO WRAPPER \ 1 2 3 4 FAILURE
CR .S \ should be empty

: EVEN? 2 MOD 0= ;

: FIRST-EVEN
  >R
  DUP EVEN? IF . SUCCEED THEN
  FAIL
;

: FIND-EVEN
  ' FIRST-EVEN SWAP PUSH-RETURN ONE-TO-FOUR
;

' FIND-EVEN WRAPPER \ 2 SUCCEED

CR .S \ should be empty

\ The following code is inspired by a EuroForth paper.
\ https://web.archive.org/web/20190107141634/http://www.complang.tuwien.ac.at/anton/euroforth/ef99/gassanenko99b.pdf

\ Let's try something else.  This backtracking system has only three
\ amazing primitives, ENTER, SUCC and FAIL.

: ENTER >R ;
: SUCC ' R@ , ' ENTER , ; IMMEDIATE
: FAIL ' R> , ' DROP , ' EXIT , ; IMMEDIATE

: RANGE ( low high "name" -- )
  1+ >R 1-
  CREATE
  ' LIT ,
  ,
  POSTPONE BEGIN
    ' 1+ ,
    ' DUP ,
    ' LIT ,
    R>
    ,
    ' < ,
  POSTPONE WHILE
  POSTPONE SUCC
  POSTPONE REPEAT
  ' DROP ,
  POSTPONE FAIL
  ' EXIT ,
;

\ 1-10 can be thought of as the non-deterministic numbers from 1 to
\ 10.
1 10 RANGE 1-10

\ Succeed or fail depending on whether the number on the stack is
\ divisible by 2.
: //2 EVEN? IF SUCC ELSE FAIL THEN ;

\ Take the numbers from 1 to 10, filter the even ones and print them.
: .even1-10 1-10 //2 DUP . ;

.even1-10 CR \ 2 4 6 8 10

\ Magic!

\ Let's do something a little bit more complicated: subsets.

: el  R@ ENTER DROP ;

\ Note: ?DO is not implemented yet at the time of writing, so this
\ will suffice.
: .{} CR ." { " DEPTH 0 2DUP = IF 2DROP ELSE DO I PICK . LOOP THEN ." } " ;

: subsets 1
          el 2
          el 3
          el .{} ;

PAGE subsets

(

You should get the following output:
 
 { 3 2 1 }
 { 2 1 }
 { 3 1 }
 { 1 }
 { 3 2 }
 { 2 }
 { 3 }
 { }

)

\ That's about it!
SHUTDOWN
