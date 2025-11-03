;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; EXERCISE

1.
What kind of Data Types are the following expressions ?
a. Atom, b. List, c. No Lisp-Expression

(X Y) Z)
()
7	
(IS THIS AN ATOM?)


2.
Write a function call that adds the numbers 15 and 7.

Write a function call that adds the integers 4, 2, and -3.

Write a function call that calculates the difference between the sum of the numbers 4 and 2 and the product of the numbers 1 and 3.
(Example: Write a function call that returns the product of the sum of the numbers 1 and 
5 and the difference of the numbers 20 and 10.
Example Solution: (* (+ 1 5) (- 20 10)) )

Write a function call that forms the quotient from the sum of the numbers 17 and 8 and the number 5. 
(Example: Write a function call that returns the quotient of the numbers 20 and 2.
Example Solution: (/ 20 2) )

3.
What is the result of evaluating the expression?
(ERROR, if the evaluation will result in an error!):

(FIRST (REST (REST '((A B C) (D E F)))))
(REST (REST '(1 2 3 4)))
(REST (FIRST (FIRST '(((A B) (C D)) (E F)))))
(FIRST (REST (FIRST (REST '((A B C) (D E F))))))
(REST (REST '((1 2) (3 4) (5 6))))
(FIRST (REST (REST (FIRST '((A B (C D)) (E F))))))
(FIRST (REST (FIRST '((A B C) (D E F)))))
(REST (FIRST (FIRST '((A B (C D)) (E F)))))

4.
Write a function call that takes the third element out of the list (A B C).
(Example: GET-SECOND-ELEMENT
Write a function call that gets the second element of the list (BROT KAFFEE MILCH ZUCKER).
Example Solution: (FIRST (REST '(BROT KAFFEE MILCH ZUCKER))) )