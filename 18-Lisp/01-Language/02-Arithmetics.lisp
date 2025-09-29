;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; ARITHMETICS

;;; Addition

(+ 2 2) ; => 4

#|
The computer sees the opening parenthesis and realizes it is being 
given a list. When it reaches the closing parenthesis it is able to 
work out that it has seen a list of three elements. The first is the 
+ sign, so it knows to add together the values of the remaining items 
on the list. So it evaluates them, the number 2 having the value 2 as 
before. Answer: 4
|#

(+ 1 2 3 4) ; => 10

(+ 1 1 1 1 1 1 1) ; => 7

(+ 1 20 300 4000 50000) ; => 54321


#|
Instead of writing 1+20+300+4000+50000, one writes the plus sign as the 
first item of a list that can be as long as you wish.
The list appears to be laid out like one might lay out a shopping list: 
(potatoes carrots onions bread milk) with no concession to the idea that 
+ is part of arithmetic and a bit special. Be careful though. The first
 location on the list is special, and + has to come first.
|#

;;; Multiplication

;; For multiplication we use the ‘*’ function.
(* 5 7) ; => 35

;; Can you use a list as long as you want? Yes.
(* 2 2 2) ; => 8
 
(* 5 7 11) ; => 385

(* 1 1 5 1 1 1 7 1 1 1 11) ; => 385

(* 1 1 5 1 1 1 7 0 1 1 11) ; => 0

;; In fact, you can use lists as short as you want.
(+ 23) ; => 23

(* 137) ; => 137

;;; Subtraction

(- 96) ; => -96

(- 96 23) ; => 73

(- 96 20 1 1 1) ; => 73

;; In other words,
(- 10 9 8 7 6) ; => -20

;; is the same as
(- 10 (+ 9 8 7 6)) ; => -20


;;; Division

;; Lisp does fractions
(+ 1/2 1/2) ; => 1

(+ 1/2 1/3) ; => 5/6

(+ 1/10 1/15) ; => 1/6

(- 1/2 1/3) ; => 1/6

(* 1/10 1/15) ; => 1/150

(/ 1/10 1/15) ; => 3/2

#|
This is potentially confusing:
If you try (/ 2 3) you get 2/3 which is probably an unpleasant surprise, 
if you were expecting 0.6666667. 
If you had tried (/ 8 12) you would also have got 2/3, 
which might have been a pleasant surprise. 
If you don't want a fraction, you can always say:
|# 

(float 8/12) ; => 0.6666667
; or
(float (/ 8 12)) ; => 0.6666667
; or
(* 1.0 (/ 8 12)) ; => 0.6666667

;; Division works the same way as subtraction with
(/ 1 2 3 4 5) ; => ; => 1/120
;; being the same as
 (/ 1 (* 2 3 4 5)) ; => 1/120

;;This works OK in practice. A calculation such as 6×5×4/(3×2×1) 
gets translated to the following piece of Lisp:
(/ (* 6 5 4) 3 2 1) ; => 20

;;; Modulo

;; Integer rest of division
(mod 23 12) ; => 11

;;; Exponential & Logarithmic Functions

;;Returns base-number raised to the power.

(expt 2 3) ; => 8

;; Returns the logarithm of number in the base base.

(log 8.0 2) ; => 3.0 
(log 100.0 10) ; => 2.0

;; Returns the principal square root of number.

(sqrt 9.0) ; => 3.0 
(sqrt 12) ; => 3.4641016

;;; Rounding

(round 10.6) ; => 11 , -0.39999962
(round 10.6 2) ; => 5 , 0.6000004
(floor 10.6) ; => 10 , 0.6000004
(rem 10.6 2) ; => 0.6000004

;;; Greatest Common Divisor

(gcd 4 8 16) ; => 4

;;; Minimum / Maximum

(min 80 56 78 62) ; => 56
(max 80 56 78 62) ; => 80



