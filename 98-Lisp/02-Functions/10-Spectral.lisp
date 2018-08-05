;;; -----------------------------------------------------------------
;;; LISP CAC Tutorial - Introduction to Common Lisp
;;; Studio for Electronic Music (SEM), University Mozarteum, Salzburg
;;; (c) 2016-2017, Achim Bornhoeft
;;; -----------------------------------------------------------------

;;; SPECTRAL

;;; code from: http://subsynth.sourceforge.net/midinote2freq.html
(defun midi2hertz (mid)
  "Convert midi to hertz."
  (* (/ 440 32) (expt 2 (/ (- mid 9) 12))))

;; (midi2hertz 60) => 261.62555

;;; code from: http://www.musicdsp.org/showone.php?id=125
(defun hertz2midi (hertz &optional (precision 1))
  "Convert hertz to midi, precision 1 = semtiones, 0.5 = quartertones."
  (* 1.0 precision (round (+ 69.0 (* 12 (log (/ hertz 440) 2))) 
                          precision )))

;; (hertz2midi 220) => 57
;; (hertz2midi 454.536 4) => 70
;; (hertz2midi 454.536 0.5) => 69.5
;; (hertz2midi 454.536 1/3) => 69.666664

;;; Class Exercise
;; Change the argument in both functions to accept numbers AND list.

(defun midi2hertz (midis)
  "Convert midi to hertz."
  (let ((mi (if (numberp midis) (list midis) midis)))
    (loop for i in mi collect
      (* (/ 440 32) (expt 2 (/ (- i 9) 12))) into reslis
      finally (return (if (numberp midis) (first reslis) reslis)))))

;; (midi2hertz 60) => 261.62555
;; (midi2hertz '(34 76 78 60)) => (58.270466 659.2552 739.98883 261.62555)

;;; Class Exercise
;; Write a function to calculate the combination tones of two frequencies.
;; Add optional argument to ouput either the difference tones only or including the
;; original frequencies.
;; Add optional midi input and output

(defun combtones (freq1 freq2)
  (list (+ freq1 freq2) 
        (abs (- freq1 freq2))))

(defun combtones (freq1 freq2 &key (all t))
  (let ((coto (list (+ freq1 freq2) 
               (abs (- freq1 freq2)))))
    (if all 
      (append (list freq1 freq2) coto)
      coto)))

;; (combtones 440 442)
;; (combtones 440 442 :all nil)

(defun combtones (freq1 freq2 &key (type :all))
  (let ((coto (list (+ freq1 freq2) 
               (abs (- freq1 freq2)))))
    (case type
      (:all (append (list freq1 freq2) coto))
      (:sum (first coto))
      (:diff (second coto))
      (:comb coto))))

;; (combtones 440 442) => (440 442 882 2)
;; (combtones 440 442 :type :all) => (440 442 882 2)
;; (combtones 440 442 :type :sum) => 882
;; (combtones 440 442 :type :diff) => 2
;; (combtones 440 442 :type :comb) => (882 2)

(defun combtones (val1 val2 &key (type :all) (mid t))
  (labels ((midi2hertz (mid)
             "Convert midi to hertz."
             (* (/ 440 32) (expt 2 (/ (- mid 9) 12))))
          (hertz2midi (hertz &optional (precision 1))
            "Convert hertz to midi, precision 1 = semtiones, 0.5 = quartertones."
            (* 1.0 precision (round (+ 69.0 (* 12 (log (/ hertz 440) 2))) 
                                    precision))))
  (let* ((freq1 (if mid (midi2hertz val1) val1))
         (freq2 (if mid (midi2hertz val2) val2))
         (coto (list (+ freq1 freq2) 
                     (abs (- freq1 freq2))))
         (cotom (mapcar #'hertz2midi coto)))
    (case type
      (:all (if mid (append (list val1 val2) cotom)
                     (append (list freq1 freq2) coto)))
      (:sum (if mid (first cotom) (first coto)))
      (:diff (if mid (second cotom) (second coto)))
      (:comb (if mid cotom coto))))))

;; (combtones 69 81) => (69 81 88.0 69.0)
;; (combtones 440 442 :mid nil) => (440 442 882 2)

;;; Class Exercise
;; Write a function which does frequency modulation 
;; (fm-mod carrier modulator index)

(defun fm-mod (carrier modulator index)
  (loop for i from 1 to index
    for x = (abs (- carrier (* i modulator)))
    for y = (+ carrier (* i modulator))
    when (not (zerop x)) collect x
    when (not (zerop y)) collect y))

(fm-mod 200 300 2)

;;; Class Exercise
;; Write a function which does frequency modulation with ratios
;; (fm-mod-ratio frequency c-ratio m-ratio index)






  