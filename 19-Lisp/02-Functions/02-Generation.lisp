;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; GENERATION

;; "integer-lst" a series of integers between two values.
;; (integer-lst 2 12) => (2 3 4 5 6 7 8 9 10 11 12)

(defun integer-lst (start end)
  (loop for x from start to end collect x))

(integer-lst 2 12)

;; "palidrome-lst" generate a palindrom version of a list
;; (palindrome-lst 1 5) => (1 2 3 4 5 4 3 2 1)
;; NOTE: Could be also (1 2 3 4 5 5 4 3 2 1).

(defun palindrome-lst (start end &key (type 'even))
  (let ((lis (loop for x from start to end collect x)))
    (case type
      (even (append lis (reverse lis)))
      (odd (append lis (reverse (butlast lis)))))))

 (palindrome-lst 2 11)
 (palindrome-lst 2 11 :type 'odd)

;; do the same with list as input argument.

(defun palindrome-lis (lis &key (type 'even))
    (case type
      (even (append lis (reverse lis)))
      (odd (append lis (reverse (butlast lis))))))

(palindrome-lis '(pp p f mf fff))

;; "repeat-item" collect a repeated item in a list
;; (repeat-item 4 10) ; (4 4 4 4 4 4 4 4 4 4)

(defun repeat-item (item reps)
  (loop repeat reps collect item))

(repeat-item 'pp 20)

(defun repeat-item-lst (item-lst reps)
  (loop repeat reps append item-lst))

(repeat-item-lst '(2 3 4) 5)

(defun repeat-items (items reps &key (type 'full))
  "Documentation string."
  (let ((item-lis (if (numberp items) (list items)
                    (if (listp items) items
                      (if (symbolp items) (list items)
                      (error "Wrong type! - Idiot!!!"))))))
    (case type
      (full (loop repeat reps append item-lis))
      (total (loop repeat reps 
               for i from 0 collect 
               (nth (mod i (length item-lis)) item-lis))))))

(repeat-items '(2 3 4) 5)
(repeat-items 4 10)
(repeat-items '(pp f mf) 12)
(repeat-items '(2 3 4 5) 19 :type 'total)

;; loop-repeat
;; collect until a specific total number of events.
;; (loop-repeat '(1 2 3 4) 10) => (1 2 3 4 1 2 3 4 1 2)
;; use nth and mod for this.










