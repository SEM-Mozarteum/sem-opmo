;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; INPUT / OUTPUT

;; Write line by line to file 
;; not overwriting existing ones with the same name
(with-open-file (stream "~/Desktop/myfile1.txt" 
                        :direction :output)
(format stream "This is the first line.") 
(terpri stream)
(format stream "This is the second.") 
(terpri stream)
(format stream "... and a third one."))

;; Overwrite file
(with-open-file (outfile "~/Desktop/myfile2.txt" 
                        :direction :output
                        :if-exists :new-version) ; overwrite file
(loop for i from 0 to 30 do (format outfile "~a~%" i)))

;; Read from file

(with-open-file (infile "~/Desktop/myfile2.txt" 
                        :direction :input
                        :if-does-not-exist nil)
  (when infile
    (loop for line = (read-line infile nil)
      while line do (format t "~a~%" line))))

;; Make a list from numbers in a file
(defun read-file (filename)
  (with-open-file (infile filename)
    (loop for num = (read infile nil)
          until (null num)
          collect num)))

(setf numblst
(let ((data (read-file "~/Desktop/myfile2.txt")))
  (loop for i in data collect (+ i 10))))


;; Format loop

(defparameter listoffour '(0 261.62555 1 883 1 184.99721 1 1835 6 174.61412 1 766 7 130.81278 1 1918 8 97.99885 1 2135 9 82.40688 1 1538 10 92.498604 1 849 11 146.83238 1 817 16 329.62756 1 925 17 391.99542 1 826 18 415.3047 1 2154 19 349.22824 1 1563 20 277.18265 1 1005 21 293.66476 1 1582 27 554.3653 1 1788 29 659.2551 1 854 31 783.99085 1 1900 33 987.7666 1 1532 35 1108.7306 1 1897 18 415.3047 0 15011 17 391.99542 0 6993 25 523.2511 0 15634 28 587.3295 0 17259 31 783.99085 0 12781 32 880.0 0 7614 33 987.7666 0 7378 35 1108.7306 0 8188 34 1046.5022 0 7446 19 349.22824 0 17405 22 369.99442 0 12970 23 440.0 0 8791 3 138.59133 0 13118 7 130.81278 0 14659 4 123.470826 0 7657 2 110.0 0 15497 8 97.99885 0 12741 10 92.498604 0 15475))


;; Divide the list in groups of 4 and put a semicolon at the end
(with-open-file (stream "~/Desktop/le1-cues.txt" 
                        :direction :output
                        :if-exists :new-version)
  (format stream  
          (format nil "~{~a ~a ~a ~a~^; ~}~%" listoffour)))


(setf lst (loop for i in (gen-tendency 100 '(-100 -60 -65 -20 -8 -14 0.01) :seed 123)
                for n in (gen-tendency 100 '(-100 -60 -65 -20 -8 -14 0.01) :seed 321)
                for j from 1 to 100
                append  (list j i n)))

;; Divide list in groups of 3, put a semicolon at the end and switch to newline
(with-open-file (stream "~/Desktop/tendencies.txt" 
                        :direction :output
                        :if-exists :new-version)
   (format stream  (format nil "~{~a, ~a, ~a~^;~% ~}" lst)))




 