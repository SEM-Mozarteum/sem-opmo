;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; INPUT / OUTPUT

;; Write to file
(with-open-file (stream "~/Desktop/myfile1.txt" 
                        :direction :output)
(format stream "Welcome to Tutorials Point!") 
(terpri stream)
(format stream "This is a tutorials database") 
(terpri stream)
(format stream "Submit your Tutorials, White Papers and Articles into our Tutorials Directory."))

;; Overwrite file
(with-open-file (outfile "~/Desktop/myfile2.txt" 
                        :direction :output
                        :if-exists :new-version)
(loop for i from 0 to 60 do (format outfile "~a~%" i)))

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

(let ((data (read-file "~/Desktop/myfile.txt")))
  (loop for i in data collect (+ i 10)))


 