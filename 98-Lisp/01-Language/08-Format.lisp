;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; FORMAT

;;; "The FORMAT language isn't Lispy at all". 
;;; Peter Seibel, A Few FORMAT Recipes

;; T is shorthand for the stream *STANDARD-OUTPUT*

(format t "~a" 'eggs) ; ASCII
(format t "~d" 1.234) ; Decimal
(format t "~$" pi) ; Decimal (with 2 digits)
(format t "~4$" pi) ; Decimal (with 4 digits)
(format t "~f" pi) ; Float

;; NIL causes FORMAT to generate its output to a string.

(format nil "The value is: ~a" 10)           ==> "The value is: 10"
(format nil "The value is: ~a" "foo")        ==> "The value is: foo"
(format nil "The value is: ~a" (list 1 2 3)) ==> "The value is: (1 2 3)"

(setf symblis '(eggs bread butter carrots))

(format t "~{~a~^, ~}.~%" symblis); Loop between {}
;; => eggs, bread, butter, carrots.

;; Note that not only is the list of values iterated over directly 
;; by format, but the commas correctly are printed between items, 
;; not after them. 

(setf numblis '(1 3 2 4 3 5 4 6))

(loop for i in numblis
  do (format t "This is line ~d.~%" i))