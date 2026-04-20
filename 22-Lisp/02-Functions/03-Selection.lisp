;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; SELECTION

;; last-number return the last element of a list.
;; (last-number '(5 6 4 7 8 3 4)) => 4
;; NOTE: Inbuild function: (last '(5 6 4 7 8 3 4)) => (4)
;; Version 1: nth element with length of list.
;; Version 2: reverse the list and take the first element.
;; Version 3: take the first element of last.

(defun last-number (lst)
  (first (last lst)))

(last-number '(1 2 3 4 5 6 7))

;; nth-elements returns the elementscof a list at 
;; a given position
;; (nth-elements  '(2 4 6 8) '(3 9 6 5 7 1 3 4 6 4 7 8))

(defun nth-elements (pos-lis lis)
  (loop for i in pos-lis collect (nth i lis)))

 (nth-elements  '(2 4 6 8) '(3 9 6 5 7 1 3 4 6 4 7 8))

;; (position 5 '(3 6 4 5 7 8)) => 3
;; Returns the list index of 5 in the list.
;; Rewrite it as "index-no"
;; Return all indexes of number in a list.

(defun index-no (item lst)
  (loop for i in lst
    for y from 0
    if (equal i item) collect y))

(index-no 5 '(4 3 5 6 7 5 8 9 5 3 4 2)) ; => (2 5 8)
;; Compare to inbuild function:
(position 5 '(4 3 5 6 7 5 8 9 5 3 4 2)) ; => 2


;;; Class Exercise

;; Write a function that returns either the first, the last 
;; or all indexes of a given value in a list
;; (indexes 3 '(5 4 6 3 7 6 3 4 5 3 2) :type :end) => 9

(defun indexes (val lis &key (type :all))
             "Returns the list index of key in row."
  (let* ((results (loop for i in lis
                 for y from 0
                 when (= i val) collect y))
         (lres (length results)))
    (if (equal results nil)
      (format t "Value ~d is not in list!" val)
      (case type
        (:start (first results))
        (:end (first (last results)))
        (:all (if (= lres 1) (first results) results))))))
                 
          
;; (indexes 3 '(2 5 4 6 3 7 8 3))
;; (indexes 9 '(2 5 4 6 3 7 8 3))
;; (indexes 3 '(2 5 4 6 3 7 8 3) :type :end)
;; (indexes 3 '(2 5 4 6 3 7 8 3) :type :start)
