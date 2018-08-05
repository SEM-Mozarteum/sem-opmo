;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; MODIFICATION

;; Permutation

;; function for 1-step permutation (left)
;; (left-rot '(1 2 3 4 5)) => (2 3 4 5 1)

(defun left-rot (lis)
  (append (rest lis) (list (first lis))))

;; function for 1-step permutation (right)
;; (right-rot '(1 2 3 4 5)) => (5 1 2 3 4)

(defun right-rot (lis)
  (append (last lis) (butlast lis)))

;; function for permutation by number
;; combine nth with mod to access the list
;; (list-permute '(1 2 3 4 5 6) 0) => (1 2 3 4 5 6)
;; (list-permute '(1 2 3 4 5 6) 2) => (3 4 5 6 1 2)
;; (list-permute '(1 2 3 4 5 6) -2) => (5 6 1 2 3 4)

(defun list-permute (lis start)
(loop repeat (length lis)
               for i from start
               collect (nth (mod i (length lis)) lis)))

;;; Class Exercise
;; Include the list-permute function via labels into a new function
;; in which the start argument is actually the last permutation of 
;; the list beginning from 0.

;;; Class Exercise
;; Add the functionality to stop the process after n events.

;; Scaling

;; (scaling-max '(1 2 3 4 5 6) 10.0) 
;; => (1.6666666 3.3333333 5.0 6.6666665 8.333333 10.0)
;; get the new scaling factor by dividing the new by the old max

(defun scaling-max (lis max)
  (let* ((old-max (apply #'max lis))
         (fact (/ max old-max)))
    (loop for i in lis collect (* i fact))))

;; (scaling-max '(1 2 -3 4 5 6) 10.0)

;; Examples for scaling max in OPMO
;; Comparison between original and scaled chord

(setf chord '(-2 4 7 -1 8 -7 9))
(setf scaled-chord (mapcar #'round (scaling-max chord 18)))
(setf new-chord (chordize (integer-to-pitch scaled-chord))) 
(setf 2chords (list (chordize (integer-to-pitch chord)) new-chord))

;; Transition of scaled chords

(setf scaled-transition
      (matrix-transpose
       (loop for i from 10 to 20
         collect (integer-to-pitch 
                  (mapcar #'round
                          (scaling-max chord i))))))

;; show transition visually in Opusmodus:

(pitch-list-plot scaled-transition :join-points t)
