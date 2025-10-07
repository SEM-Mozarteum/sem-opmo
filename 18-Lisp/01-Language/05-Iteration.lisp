;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; ITERATION

;; There is an iteration macro in Lisp called LOOP:

(loop for x from 0 to 12 collect x)
(loop for integers from 0 to 12 collect integers) ; => (0 1 2 3 4 5 6 7 8 9 10 11 12)

(loop for m from 60 to 72 collect m)
(loop for midinote from 60 to 72 collect midinote) ; => (60 61 62 63 64 65 66 67 68 69 70 71 72)

(loop for i in '(60 61 62 63 64 65) collect (+ i 12))

(defparameter  midis '(60 61 62 63 64 65 66 67 68 69 70 71 72))
(loop for i in midis collect (+ i 12))

;; There a many keywords available for LOOP:

(loop for pc from 60 to 84 by 2 collect pc) ; => (60 62 64 66 68 70 72 74 76 78 80 82 84)

;; harmonic spectrum (hertz)

(loop for i from 2 to 14 collect (* 23 i))

(loop for pc from 84 downto 60 by 4 collect pc) ; => (84 80 76 72 68 64 60)

(loop for freq = 40 ; freq is the loop variable which starts with c1
  then (* freq 1.1) ; and continue by duplicating the last result
  repeat 8 ; repeat this 8 times
  collect freq) ; and collect each value in a list

;; Nested loops are created like this:

(loop for i from 1 to 5 collect
  (loop for j from 0 to 4 collect (+ i j)))
; => ((1 2 3 4 5) (2 3 4 5 6) (3 4 5 6 7) (4 5 6 7 8) (5 6 7 8 9))

;; rearrange a list by taking each position in a seperate list (matrix transformation)

(defparameter lists '((1 2 3 4 5) (3 4 5 6 7)  (5 6 7 8 9)))

(loop for i from 0 to 4 
  ; with lists = '((1 2 3 4 5) (3 4 5 6 7)  (5 6 7 8 9))
  collect
  (loop for j in lists collect (nth i j))) ; nth position in list

;;; -----------------------------------------------------------------

;;; There are two older and therefore less commonly used loop macros in LISP:

;; Dolist binds a variable to the elements of a list in order and stops 
;; in the end of the list.

(dolist (x '(60 61 62 63)) (print x))

;; DOLIST always returns NIL. Note that the value of x in the above example was never NIL: 
;; the NIL below the 63 was the value that dolist returns.

;; DOTIMES is like dolist except that it iterates over integers.

(dotimes (i 4) (print (* i i)))


#|
Common Lisp also has several forms (functions and macros) for operating on the contents of lists. mapcar can be used to process each element of a list one at a time to build up a new list.
The #' is a special way to pass a function as an argument into a function. 
The function must then take the same number of arguments as there are lists:
|#

(mapcar #'+ '(1 2 3) '(4 5 6)) ; ⇒ (5 7 9)

;; It is possible to write own functions to process over a list

(defun add2 (n)
    "add 2." ; description
    (+ 2 n))

(mapcar #'add2 '(1 2 3)) ;  ⇒ (3 4 5)
;; or with a loop
(loop for i in '(1 2 3) collect (+ i 2)) ; => (3 4 5)

;;; Length functions
;; in LISP
(loop for d in '(4 8) collect
  (loop for n in '(1 4 8) collect (/ n d)))
;; => ((1/4 1 2) (1/8 1/2 1))

;; aequidistant divisions of a quarter
;; in a nested loop construction
(loop for d in '(1 2 3 4 5 6 7 8 9) collect 
  (loop repeat d collect (/ 1 (* d 4))))


