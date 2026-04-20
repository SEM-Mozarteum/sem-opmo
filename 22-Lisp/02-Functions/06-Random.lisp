;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; RANDOM

;; Centered random

;; Make a list that returns random values between low and high
;; including them.

(defun rand-lohi (low high)
  (let ((dif (+ 1 (abs (- high low)))))
    (+ low (random dif))))

;; (rand-lohi 3 7)


;; Weighted random

#|
First make a function with a list of values from a list of paired numbers
where the first is the number to be chosen at random and the second is
the percentage e.g. '(1 60 2 30 3 10)) - means 60% number 1, 30% number 2 
and 10% number 3. The list should then consist of 100 values: 60 x 1, 
20 x 2 and 10 x 3.
|#

(defun weightlis (wlis)
  "Make a list of elements from a paird entry list."
(loop for i from 0 below (length wlis) by 2
  append (loop repeat (nth (+ i 1) wlis)
            collect (nth i wlis))))

;; (weightlis '(1 60 2 30 3 15))

#|
(length '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3)) => 100 = 100 %
If there are more or less elements than 100 the function works accordingly.
|#  


;;; Class Exercise
;; Write a weighted random function that takes n elements from a list of
;; weighted numbers. Therefore put the loop part of weightlis in a let
;; variable and then chose from this list randomly with a loop

(defun weighted-lis (wlis n)
   (let ((wl (loop for i from 0 below (length wlis) by 2
  append (loop repeat (nth (+ i 1) wlis)
            collect (nth i wlis)))))
     (loop repeat n collect
       (nth (random (length wl)) wl))))
     
;; (weighted-lis '(1 60 2 30 3 15) 20)


;;; Class Exercise
;; What can you do to provide a higher precision e.g. 54.32 % ?

(defun weighted-lis1 (wlis n &optional (dig 2))
   (let ((wl (loop for i from 0 below (length wlis) by 2
  append (loop repeat (floor (* (nth (+ i 1) wlis) (expt 10 dig)))            
            collect (nth i wlis)))))
     (loop repeat n collect
       (nth (random (length wl)) wl))))

;; (weighted-lis1 '(1 60.345 2 30.123 3 10.456) 200 3)

(make-omn
 :pitch
(midi-to-pitch 
 (weighted-lis1 '(60 60.345 64 30.123 77 10.456) 200))
:length '(1/64)
:span :pitch)

;;; Class Exercise
;; Write a function to get random elements from a list without returning them

;; (urn '(1 2 3 4 5 6 7 8 8 8)) => (5 7 8 2 1 6 4 3)
;; (urn '(1 2 3 4 5 6 7 8) 5) => (8 4 1 5 6)

(defun urn (lis)
  (let* ((llis (length lis)))
    (loop repeat llis
      with restlis = lis
      with n
      do (setf n (nth (random (length restlis)) restlis))
      do (setf restlis (remove n restlis :count 1))
      collect n)))

(defun remove-nth (sequence id)
  (remove (elt sequence id) sequence :count 1))

;; (remove-nth '(1 2 3 4 4 5 3 6 4 4) 3)

;; function with removing the id's, not the numbers directly

(defun urn1 (lis)
  (let* ((llis (length lis)))
    (loop repeat llis  
      with restlis = lis
      with id
      with n
      do (setf id (random (length restlis)))
      do (print (setf n (nth id restlis)))
      do (setf restlis (remove (elt restlis id) restlis :count 1))    
      collect n)))

;; (urn1 '(1 2 3 4 5 6 7 8 8 8)) => (5 7 8 2 1 6 4 3)





 








