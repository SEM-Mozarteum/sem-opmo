;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; FILTER DATA

;;; With filtering data of any kind, the same musical material can be
;;; used in different ways.

(defun lowpass-random (n)
  "Linear random low-pass distribution (original code by Heinrich Taube)."
 (min (random n) (random n)))

;; (loop repeat 20 collect (lowpass-random 100))

;;; Class Exercise
;; Make a similar highpass function.

(defun highpass-random (n)
  "Linear random low-pass distribution (original code by Heinrich Taube)."
 (max (random n) (random n)))

;; (loop repeat 20 collect (highpass-random 100))

;;; Class Exercise
;; Make an extention with a cut-off frequency

(defun lowpass-random-cut (n cut-off)
  "Linear random low-pass distribution (original code by Heinrich Taube)."
  (let ((val  (min (random n) (random n))))
  (when (<= val cut-off) val)))

;;; Class Exercise
;; Add the possibility of a Filter-Q to this function.
;; Note: Increase the number of filtered values to be chosen from
;; choose the lowest / highest value from this list at the end

(defun highpass-random-q (n q)
  (apply #'min 
         (loop repeat q collect (max (random n) (random n)))))

;; (loop repeat 30 collect (highpass-random-q 100 5))

(defun lowpass-random-q (n q)
"random-lowpass with variable filter-q."
    (if (= q 0) (random n)
  (loop repeat q collect (random n) into numberlist
       finally (return (apply #'min numberlist)))))
;; By adding more random values the probability for lower numbers is increasing
;; (loop repeat 50 collect (lowpass-random-q 50 0))

;;; Class Exercise
;; Make an extension of the function to lowpass a list as well.

(defun lowpass-list (data)
  (apply #'min (print (loop repeat 2 collect
  (nth (random (- (length data) 1)) data)))))

;; (lowpass-list '(12 45 34 56 78 78 90 45 34 67 12 23 78 89))

(defun lowpass-list-q (data q)
  (apply #'min
  (loop repeat q collect (apply #'min (print (loop repeat 2 collect
  (nth (random (- (length data) 1)) data)))))))

;; (lowpass-list-q '(12 45 34 56 78 78 90 45 34 67 12 23 78 89) 5)

;;; Class Exercise
;; Extend the function to get more values
;; NOTE: Put the last function in a repeat loop.

(defun lp-list-q (data q rep)
  (loop repeat rep collect 
    (apply #'min
  (loop repeat q collect (apply #'min (print (loop repeat 2 collect
  (nth (random (- (length data) 1)) data))))))))

;; (lp-list-q '(12 45 34 56 78 78 90 45 34 67 12 23 78 89) 3 20)


;;; Class Exercise
;; Write a function that remove all numbers evenp, oddp, 
;; zerop, plusp.
;; use: (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) 

;; (remove-type '(1 2 3 4 5 6 7) :evenp)















