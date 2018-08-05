;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; TESTING

;; function "same"
;; (same '(2 2 2)) => t
;; (same '(2 2 3)) => nil
;; Extension: if t return x, if nil return y

(defun same (lis)
  "Check if all elements in a list are the same."
  (loop for i in (rest lis)
    collect (equal (first lis) i) into reslis
    finally (return (if (member nil reslis) nil t))))

;; (same '(1 2 1 1)) => NIL
;; (same '(1 1 1 1)) => T

(defun greater-in-list (lis val)
  "Check if there is a number greater than val in a list."
  (loop for i in lis 
    collect (> i val) into reslis
    finally (return (if (member t reslis) t nil))))

;; (greater-in-list '(23 54 67 87 23 45) 70) => T
;; (greater-in-list '(23 54 67 87 23 45) 100) => NIL


;;; Class Exercise
;; Function that returns all odd numbers from a list.

(defun return-all-odd (lst)
  (loop for i in lst
    when (oddp i) collect i))

;; (return-all-odd '(3 4 5 2 5 7 3 12 4 6 8 9 5 25 6)))

;;; Class Exercise
;; Function that returns how many odd numbers are in a list.

(defun how-many-odd (lst)
  "Return how many odd numbers are in a list."
  (loop for i in lst
    when (oddp i) collect i into reslis
    finally (return (length reslis))))

;; (how-many-odd '(3 4 5 2 5 7 3 12 4 6 8 9 5 25 6))





