;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; EXERCISE

;; "harmonics" will generate a harmonic scale from a
;; given fundamental (hz) to a nth overtone
;; (harmonics 100 5) => (100 200 300 400 500) 

(defun harm-series (fund start end step)
  (loop for i from start to end by step
    collect (* fund i)))

;; (harm-series 50 3 12 2) => (150 250 350 450 550)

;; Try to find a function that converts hz to midi notes and
;; write a function for it.

