;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; LISP for Pure Data

;; Format a [qlist] sequencing file for PD

(with-open-file (stream "~/Desktop/step.txt" 
                        :direction :output
                        :if-exists :new-version)
  (let ((lis (list 200 1000 3000 1000 1000 2000 1000 1000)))
    (loop for i in lis
      for j from 0
      do (format stream "~a send1 ~a; send2 ~a;~%" i j (+ 1 j)))))


;; Format a random wavetable for PD

(with-open-file (stream "~/Desktop/array.txt" 
                        :direction :output
                        :if-exists :new-version)
 
    (loop repeat 44100
      for j = (- 1 (random 2.0))
      do (format stream "~a~%" j)))




