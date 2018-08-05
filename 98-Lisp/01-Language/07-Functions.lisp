;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; FUNCTIONS
 
#|
A function is a concept that is encountered in almost every programming language.
Functions are most often created using the defun (DEfine FUNction) macro. 
This macro takes a list of arguments and a sequence of Lisp forms, called a 
body of a function. A typical use of defun is like this:

(defun <function-name> (<var1> <var2>)
   body))
|#

 (defun plus (x1 x2)
    (print x1) ; prints x1
    (print x2)
    (+ x1 x2))

(plus 2 3)

#|
Here, the list of arguments is (x1 x2) and the body is 
(print x1) (print x2) (+ x1 x2). 
When the function is called, each form in the body is 
evaluated sequentially (from first to last). 
|#

;; a function that converts midi to pitchclass (integers)
(defun midi2pc-1 (midi)
    (mod midi 12))

(midi2pc-1 64) ; => 4

(defun midis2pc-2 (midis)
  (loop for i in midis collect (mod i 12)))

(midis2pc-2 '(60 56 78 33 64 91)) ; => (0 8 6 9 4 7)

(defun midis2pc-3 (midis)
  (let ((mid (if (numberp midis) (list midis) midis)))
      (loop for i in mid collect (mod i 12))))

(midis2pc-3 '(60 56 78 33 64 91)) ; => (0 8 6 9 4 7)
(midis2pc-3 64) ; => 4

;; another possibility
(defun midis2pc-4 (midis)
  "Converts midi-to-pc." ; description string
  (if (listp midis)
    (loop for i in midis collect (mod i 12))
    (if (numberp midis)
    (mod midis 12)
      nil)))

(midis2pc-4 '(60 56 78 33 64 91)) ; => (0 8 6 9 4 7)
(midis2pc-4 64) ; => 4
(midis2pc-4 'c4) ; => nil

;; Optional arguments can be used to set a default argument
;; which can be changed contextually. Note that the order of
;; optional arguments is crucial e.g. to access the second
;; optional argument the first has to be given as well.

(defun hz-to-midi (freq &optional (tuning 440))
  "Convert frequencies (hz) to midi. Tuning (hz) optionally."
  (if (< freq 8) 0
(round (+ 69 (* 12 (log (/ freq tuning) 2 ))))))

(hz-to-midi 440)
(hz-to-midi 110 420)

;; Keywords arguments can be used to set a default argument
;; which can be changed contextually. With this method the
;; argument has to be called with the keyword. Therefore
;; the order is not relevant.

(defun hz-to-midi (freq &key (tuning 440))
  "Convert frequencies (hz) to midi. Tuning (hz) optionally."
  (if (< freq 8) 0
(round (+ 69 (* 12 (log (/ freq tuning) 2 ))))))

(hz-to-midi 110 :tuning 420)



