;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; MATH

;; "average" function
;; (average '(1 2 3)) => 2
;; sum of all elements in a list 
;; then divided by the number of elements.

(defun average (lst)
  (let ((sum-lst (reduce '+ lst))
        (len-lst (length lst)))
    (* 1.0 (/ sum-lst len-lst))))

;; (average '(1 2 3 4 5 6 7 8)) ; => 4.5

;; absolute difference between val1 and val2
(defun diff (x1 x2)
  (let ((mi (min x1 x2))
        (ma (max x1 x2)))
    (abs (- mi ma))))

;; (diff -1 5) ; 6
;; (diff 7 -2) ; 9
;; (diff 2 4) ; 2
;; (diff -1 -3) ; 2

(defun midi-to-hz (mid)
(* (/ 440 32) (expt 2 (/ (- mid 9) 12))))

;; (midi-to-hz 60) ; 261.62555

(defun hz-to-midi1 (freq)
  (labels ((rounding (flt)
             (let* ((fflt (floor flt))
                    (decimals (- flt fflt)))
               (+ fflt (if (< decimals 0.5) 0 1)))))
  (if (< freq 8) 0
(rounding (+ 69 (* 12 (log (/ freq 440) 2 )))))))

;; (hz-to-midi1 8) ; 0
;; (hz-to-midi1 -1) ; 0
;; (hz-to-midi1 261) ; 60
;; (hz-to-midi1 440) ; 69

(defun hz-to-midi2 (freq &optional (tuning 440))
  (labels ((rounding (flt)
             (let* ((fflt (floor flt))
                    (decimals (- flt fflt)))
               (+ fflt (if (< decimals 0.5) 0 1)))))
  (if (< freq 8) 0
(rounding (+ 69 (* 12 (log (/ freq tuning) 2 )))))))

;; (hz-to-midi2 440) ; 69
;; (hz-to-midi2 110 420) ; 46 (instead of 45)

(defun rounding (flt)
  (let* ((fflt (floor flt))
        (decimals (- flt fflt)))
    (+ fflt (if (< decimals 0.5) 0 1))))

;; (rounding 4.3) ; 4
;; (rounding 4.5) ; 5
;; (rounding 4.657) ; 5

;; round-up: (round-up 4.3) => 5
;; rounding up decimal numbers

;; to get only the first argument of round use 
;; (nth-value 0 (round 1.234567))

(defun round-up (flt)
  (nth-value 0 (ceiling flt)))

;; round-down (round-down 4.7) => 4
;; rounding down decimal numbers

(defun round-down (flt)
  (nth-value 0 (floor flt)))

;; (round-down 2.34536)

;; combine the two functions in one with keywords

(defun round-dir (flt &key (dir 'normal))
  "Rounding in various directions: normal, up and down."
  (case dir
    (normal (nth-value 0 (round flt)))
    (up (nth-value 0 (ceiling flt)))
    (down (nth-value 0 (floor flt)))
    (otherwise (format nil "Wrong keyword!"))))

;; (round-dir 2.6445645612) 
;; (round-dir 2.2453349 :dir 'up)
;; (round-dir 2.7453349 :dir 'down)
;; (round-dir 2.7453349 :dir 'next)