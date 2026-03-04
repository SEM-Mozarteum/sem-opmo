;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; SAMPLES 

;;; Class Exercise
#|
Write a function that scales an envelope from a list of x/y values into any specified duration"

1. Loop only on each second value to scale the x-values not the y-values.
To use nth you have to access the values upto (- length 1).
2. Include a keyword for scaling the y-values as well
|#

(defun env-scale (xylist xscale &key (yscale 1))
  (loop for i from 0 to (- (length xylist) 1) by 2
    collect (* xscale (nth i xylist))
    collect (* yscale (nth (+ i 1) xylist))))
    

;; (env-scale '(0 0.2 0.5 0.8 0.8 0.2 1 0.1) 100) => (0 0.2 50.0 0.8 80.0 0.2 100 0.1)
;; (env-scale '(0 0.2 0.5 0.8 0.8 0.2 1 0.1) 100 :yscale 10) => (0 2.0 50.0 8.0 80.0 2.0 100 1.0)

(defun mat-transpose (x-ylist &key (listed t))
  (loop for x in (first x-ylist)
    for y in (second x-ylist)
    if listed
    collect (list x y)
    else 
    collect x 
    and collect y))

;; (mat-transpose '((1 2 3 4 5) (34 23 56 78 34)))
;; (mat-transpose '((1 2 3 4 5) (34 23 56 78 34)) :listed nil)

(defun multi-mat-trans (multlist)
  (let ((minlen (apply #'min 
                (loop for i in multlist 
                  collect (length i)))))
    (loop for i from 0 to (- minlen 1) 
      collect
      (loop for j in multlist collect (nth i j)))))

#|
(multi-mat-trans '((1 2 3 5) (34 23 56 78 34) (3 5 4 7 6 8 9)))
=> ((1 34 3) (2 23 5) (3 56 4) (5 78 7))

(multi-mat-trans 
'((1 2 3 4 5 3 5) 
 (34 23 56 78 34) 
 (3 5 4 7 6 8 9 23) 
 (2 34 65 76 12 89 87)))
=> ((1 34 3 2) (2 23 5 34) (3 56 4 65) (4 78 7 76) (5 34 6 12))
|#

;; see also matrix-transpose in OM and mat-trans in PWGL

;;; Class Exercise
#|
Write a function to calculate any number of 
equidistant samples between start an end.

1. The difference between start and end is the 
absolute value of its substraction. This is the
range which is divided into the specified samples
2. The value of each step is 1 divided by 
(- samples 1). You have to reduce the number
of samples by one to include the high value as 
the last element in the result list.
3. To calculate the values repeat with the number 
of samples and increment from 0 by steps and 
collect it. Add the low border of the range to 
shift it to the right place.
|#

(defun samples (start end samples &key (int t))
  "Any number of equidistant values between start an end."
  (let ((qnt (/ 1 (- samples 1)))
        (dif (abs (- end start))))
  (loop repeat samples
    for i from 0 by qnt
    for res = (* 1.0 (+ start (* i dif)))
    collect (if int (round res) res))))
    
;; (samples 20 60 9) => (20 25 30 35 40 45 50 55 60)
;; (samples 20 60 8) => (20 26 31 37 43 49 54 60)
;; (samples 20 60 8 :int nil)
;; => (20.0 25.714285 31.428572 37.142857 42.857143 48.57143 54.285713 60.0)
      

