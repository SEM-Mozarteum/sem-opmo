;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; QUANTISATION

;;; Quantisation is used to reduce the resolution of any parametric data

(defun quant (num step)
  (* (round (/ num step)) step))
;; (quant 2.3456 0.5) => 2.5
;; (quant 2.3456 0.25) => 2.25
;; (quant 2.456 0.25) => 2.5

(defun quant-up (num step)
    (let ((q (* (round (/ num step)) step)))
	  (if (< q num) (+ q step) q)))
;; (quant-up 2.2 .5) => 2.5

(defun quant-down (num step)
    (let ((q (* (round (/ num step)) step)))
	  (if (> q num) (- q step) q)))
;; (quant-down 2.2 .5) => 2.0
;; (quant-down 2.5 .6) => 2.0

;; Get the position (ID) of an element in a list
(position 4 '(3 24 5 76 1 99 49)) => NIL
(position 99 '(3 24 5 76 1 99 49)) => 5

;;; Class Exercise
;; Write a quantise function with a floating point number and a list of
;; quantisation steps as input. 
;; 1. quantize the number to all steps by multiplying the rounded division of number
;; and step by step
;; 2. Calculate all differences between the num and the quantized versions.
;; 3. Select the smallest value
;; 4. Get the position of this smallest value
;; 5. The result is the value in the list of quantized values at this position.

(defun quantise (num quant-lis)
  (let* ((qlis (loop for i in quant-lis collect (* (round (/ num i)) i)))
         (minlis (loop for i in qlis collect (abs (- i num))))
         (minval (apply #'min minlis))
         (minpos (position minval minlis)))
(nth minpos qlis)))

;; (quantise 2.3456 '(0.25 0.333))

(defun quantise-list (lis steps)
"Quantisation of a list to a defined step value (number or list of numbers."
  (loop for i in lis collect (quantise i steps)))
;; (quantise-list '(1.25346 5.43765) 0.5)

(defun quantisation (val quant)
  (let ((vallis (if (numberp val) (list val) val))
        (quantlis (if (numberp quant) (list quant) quant)))
    (labels ((quantise (num quant-lis)
               (let* ((qlis (loop for i in quant-lis collect (* (round (/ num i)) i)))
                      (minlis (loop for i in qlis collect (abs (- i num))))
                      (minval (apply #'min minlis))
                      (minpos (position minval minlis)))
                 (nth minpos qlis))))
      (loop for i in vallis collect (quantise i quantlis)))))

;; quantisation 1.2345 0.25)
;; (quantisation '(1.234234 2.34345 3.45656745) 0.25)
;; (quantisation 1.749 '(0.3 0.25 0.125))
;; (quantisation '(1.234234 2.34345 3.45656745) '(0.3 0.25 0.125))
         
(defun raster (lis grid)
  "Quantisation of a list of numbers to a fixed selection of numbers."
  (loop for i in lis collect
    (loop for j in grid
      collect (abs (- i j)) into reslis
      finally (return (nth (position (apply #'min reslis) reslis) grid)))))

(raster '(1/12 5/27 31/108 7/18 53/108 16/27 25/36 43/54 97/108 1)
        '(1/12 1/10 1/9 1/8 1/7 1/6 1/5 1/4 1/3 1/2 1 2 3 4))
;; => (1/12 1/5 1/4 1/3 1/2 1/2 1/2 1 1 1)

(raster '(1.23453 1.432516 1.654 1.4132 1.76)
        '(1 1.5 2))
;; =>(1 1.5 1.5 1.5 2)
