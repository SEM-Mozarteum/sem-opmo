;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; BINDINGS

#|
Binding is the act of specifying a place holder for a value.  
You often want to do this because it is cumbersome to write out long expressions multiple times, or if a computation needs to be done in smallparts where a binding needs to be updated at various times during execution. 
|#

;;; Global Variables

;; We start by setting a symbol's general purpose value. There are several commands for setting 
;; the values of symbols, set, setq, setf, psetq, psetf. One can get a long way with just 
;; setf so we start with that one:

(setf my-first-symbol 57)
57

;; This sets the general purpose value of the symbol MY-FIRST-SYMBOL to 57, 
;; and returns 57. Now we can type

my-first-symbol
57

;; and

(+ my-first-symbol 3)
60

(setf second-symbol (+ 20 3))
23

;; Well, plainly this has performed the calculation, and returned the answer, 
;; but what did the general purpose value of our second-symbol get set to? 
;; Have we used it to record the calculation we requested, (+ 20 3), 
;; or the answer that the computer calculated?

second-symbol
23

;; If we want to record the calculation for future reference we must "quote" it. 
;; Think of the computer as a horse and the quote as a bridle, reining it in, 
;; stopping it from rushing on to evaluate things before you want it to.

(setf lis (list 1 2 3 4 5)) ; => (1 2 3 4 5)

;; Now evaluate

lis ; => (1 2 3 4 5)

;;; Local Variables

;; The main way to create local bindings to variables  is via the “special form” LET.

(let ((5-squared (* 5 5))
      (10-squared (* 10 10)))
  (* 5-squared 10-squared))

;; => 2500

(let* ((x (+ 12 14))
       (y (+ x 4))))

#|
Here, 5-SQUARED and 10-SQUARED are place holders ("local variables") for the results of the calculation (* 5 5) and (* 10 10), respectively. It is good to note at this time that there are very few rules regarding what can be used as a place holder. These place holders are called symbols and it can have a name that includes most any characters with the exception of quotes, open or close parenthesis, colons, backslashes, or vertical bars (‘|’). These all have special syntactical meaning in Common Lisp. It is good to note that all of these things actually can be in the name of a symbol but they require special escaping.

Bindings have a limited scope. Once the LET form closes, the binding is invalidated. This means that this is an error, because a is referred to outside of the enclosing LET form.
|#

(let ((a (sqrt 100)))
  (print a))

(print a) ; => Error: Unbound variable: a

;;; Dynamic Bindings

#|
There are two types of way to make bindings in Common Lisp, lexical, which we have just seen, and dynamic. For our purposes at this point, dynamic bindings are not much different from lexical bindings, but they are made in a different way and do not have the same finite extent of the LET form. We can use DEFVAR and DEFPARAMETER to make dynamic bindings. These can hold a value in between inputs.
|#

	
;; DEFPARAMETER always assigns a value. So:

[1]> (defparameter a 1) ; => A
[2]> (defparameter a 2) ; => A
[3]> a ; => 2

;; while DEFVAR does it only once, so:

[4]> (defvar b 1) ; => B
[5]> (defvar b 2) ; => B
[6]> b ; => 1





