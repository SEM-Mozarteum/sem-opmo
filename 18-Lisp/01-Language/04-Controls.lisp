;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; CONTROLS

;;; Attributes

;; LISTP is testing if the argument is a list

(LISTP '(A B C))
T

(LISTP NIL) 
T 

(LISTP 12) 
NIL 

;; CONSP is testing if the argument is a non-empty list

(CONSP 6) 
NIL 

(CONSP '(A B)) 
T 

(CONSP NIL)
NIL

;; NUMPBERP is testing if the argument is a number

(NUMBERP 12)
T

(NUMBERP 'ZAHL) 
NIL 

;; EQUAL is testing if both arguments are the same

(EQUAL '(1 2 3) '(1 2 3)) 
T 

(EQUAL '(1 2 3) '(4 5)) 
NIL 

;; ZEROP is testing if the argument is zero

(ZEROP 0) 
T 

(ZEROP 12) 
NIL 

;; MEMBERP is testing if the argument is a member of the following list. 
;; If true the list from this point to the end is returned

(MEMBER 'I '(A I O U)) 
(I O U) 

(MEMBER 'B '(A I O U)) 
NIL 


;;; Arithmetic Comparions

(> 7 6) 
T 

(< 2 3) 
T 

(> 2 6) 
NIL 

(< 45 3) 
NIL 

(= 10 10) 
T 

(= 29 10) 
NIL


;;; Logical Operators

;; NOT is testing if the argument is evaluation to NIL 

(NOT T) 
NIL 

(NOT NIL) 
T 

(NUMBERP 3) 
T 

(NOT (NUMBERP 3)) 
NIL 

;; OR is testing from left to right and returns the first value that is not
;; evaluating to nil.

(OR 'A 'B) 
A 

(OR (NUMBERP 10) (CONSP 10) NIL) 
T 

(OR (NUMBERP '(A B C)) (CONSP '(A B C))) 
T 

(OR (NUMBERP 'A) (CONSP 'A)) 
NIL

;; AND is testing if all the arguments are evaluating to TRUE
;; It stops at the first argument evaluatin to NIL.

Beispiele: 
(AND T T T) 
T 

(AND NIL NIL NIL) 
NIL 

(and (zerop 0) (numberp 1))

(AND T NIL) 
NIL 

(AND (FIRST '(A B C)) (REST '(E F G))) 
(F G)

;;; Conditionals

#|
LISP provides a standard set of logical functions, for example and, or, and not. 
The and and or connectives are short-circuiting: and will not evaluate any arguments 
to the right of the first one which evaluates to nil, while or will not evaluate any 
arguments to the right of the first one which evaluates to t (with t we mean here ``non-nil'').

LISP also provides several special forms for conditional execution. The simplest of these is if. 
The first argument of if determines whether the second or third argument will be executed:
|#

;; if => test => then => else
(if t 5 6)
5

(if (numberp 3) 3 nil)
(if (numberp 'x) 3 'x)

(if nil 5 6)
6

(if 4 5 6)
5

#|
If you need to put more than one statement in the then or else clause of an if statement, 
you can use the progn special form. Progn executes each statement in its body, then returns 
the value of the final one.
|#

(setq a 7)
7

(if (> a 5) 
  a 
  5)


;; An if statement which lacks either a then or an else clause can be written using the when or unless special form:

(when t 3)
3

(when nil 3)
NIL

#|
A cond consists of the symbol cond followed by a number of cond clauses, each of which is a list. 
The first element of a cond clause is the condition, the remaining elements (if any) are the action. 
The cond form finds the first clause whose condition evaluates to true (ie, doesn't evaluate to nil), 
it then executes the corresponding action and returns the resulting value. None of the remaining 
conditions are evaluated, nor are any actions except the one corresponding to the selected condition. 
For example:
|#

(setq a 3)
3

(cond
 ((evenp a) a)        ;if a is even return a
 ((> a 7) (/ a 2))    ;else if a is bigger than 7 return a/2
 ((< a 5) (- a 1))    ;else if a is smaller than 5 return a-1
 (t 17))              ;else return 17

;; case for conditional variable to be used e.g. for keywords

(defun plus-minus (x1 x2 &key (type 'plus))
                     (case type
                       (plus (+ x1 x2))
                       (minus (- x1 x2))
                       (otherwise (error "Wrong keyword."))))

(plus-minus 2 1) ; 3
(plus-minus 2 1 :type 'plus) ; 3
(plus-minus 2 1 :type 'minus) ; 1
(plus-minus 2 1 :type 'whatever) ; > Error: Wrong keyword.
(
