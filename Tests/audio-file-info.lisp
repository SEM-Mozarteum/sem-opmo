;;;; audio-file-info.lisp
;;;;
;;;; Liest die Header von WAV- (RIFF) und AIFF/AIFC-Dateien und bestimmt
;;;; daraus die Dauer, OHNE die Audiodaten selbst einzulesen (nur der Header
;;;; wird gelesen, daher auch bei sehr langen Dateien schnell).
;;;;
;;;; Unterstuetzt beliebige Samplingraten, Bittiefen und Kanalzahlen.
;;;; Reines ANSI Common Lisp -- laeuft unter SBCL, CCL (Opusmodus) usw.
;;;;
;;;; Verwendung:
;;;;   (audio-file-duration "/pfad/zur/datei.wav")   ; => Dauer in Sekunden
;;;;   (audio-file-info     "/pfad/zur/datei.aiff")  ; => vollstaendige Plist

;;; ---------------------------------------------------------------------------
;;; Low-Level-Helfer
;;; ---------------------------------------------------------------------------

(defun read-uint-le (stream n)
  "Liest eine N-Byte-Ganzzahl ohne Vorzeichen in Little-Endian-Reihenfolge."
  (let ((value 0))
    (dotimes (i n value)
      (setf value (logior value (ash (read-byte stream) (* 8 i)))))))

(defun read-uint-be (stream n)
  "Liest eine N-Byte-Ganzzahl ohne Vorzeichen in Big-Endian-Reihenfolge."
  (let ((value 0))
    (dotimes (i n value)
      (setf value (logior (ash value 8) (read-byte stream))))))

(defun read-fourcc (stream)
  "Liest einen Vier-Zeichen-Chunk-Bezeichner als String."
  (let ((id (make-string 4)))
    (dotimes (i 4 id)
      (setf (char id i) (code-char (read-byte stream))))))

(defun skip-bytes (stream n)
  "Ueberspringt N Bytes im STREAM (effizient via FILE-POSITION)."
  (when (plusp n)
    (file-position stream (+ (file-position stream) n))))

(defun read-ieee-extended (stream)
  "Liest eine 80-Bit-IEEE-754-Extended-Zahl (Big-Endian) und liefert sie als
   DOUBLE-FLOAT. In diesem Format speichert AIFF die Samplingrate."
  (let* ((sign-and-exp (read-uint-be stream 2))
         (mantissa     (read-uint-be stream 8))
         (sign         (if (logbitp 15 sign-and-exp) -1 1))
         (exponent     (logand sign-and-exp #x7fff)))
    (cond
      ((and (zerop exponent) (zerop mantissa)) 0.0d0)
      ((= exponent #x7fff)
       (error "Ungueltiger (nicht endlicher) IEEE-Extended-Wert im AIFF-Header."))
      (t
       ;; Die 64-Bit-Mantisse enthaelt das explizite Integer-Bit (Bit 63):
       ;; Wert = mantissa * 2^(exponent - 16383 - 63)
       (* sign
          (scale-float (coerce mantissa 'double-float)
                       (- exponent 16383 63)))))))

;;; ---------------------------------------------------------------------------
;;; WAV / RIFF
;;; ---------------------------------------------------------------------------

(defun read-wav-info (path stream)
  "Parst einen RIFF/WAVE-STREAM. Der RIFF-Bezeichner ist bereits gelesen."
  (read-uint-le stream 4)                       ; Groesse des RIFF-Chunks
  (let ((form (read-fourcc stream)))
    (unless (string= form "WAVE")
      (error "~A ist keine WAVE-Datei (Form-Typ ~S)." path form)))
  (let (sample-rate channels bits-per-sample data-bytes)
    (handler-case
        (loop until (and sample-rate data-bytes) do
          (let ((chunk-id   (read-fourcc stream))
                (chunk-size (read-uint-le stream 4)))
            (cond
              ((string= chunk-id "fmt ")
               (read-uint-le stream 2)          ; Audioformat-Code
               (setf channels    (read-uint-le stream 2)
                     sample-rate (read-uint-le stream 4))
               (read-uint-le stream 4)          ; Byte-Rate
               (read-uint-le stream 2)          ; Block-Align
               (setf bits-per-sample (read-uint-le stream 2))
               (when (> chunk-size 16)          ; evtl. Erweiterung ueberspringen
                 (skip-bytes stream (- chunk-size 16))))
              ((string= chunk-id "data")
               (setf data-bytes chunk-size)
               (skip-bytes stream chunk-size))
              (t
               (skip-bytes stream chunk-size)))
            (when (oddp chunk-size)             ; Chunks sind auf gerade Laenge gepaddet
              (skip-bytes stream 1))))
      (end-of-file ()))
    (unless (and sample-rate data-bytes)
      (error "~A: unvollstaendiger WAVE-Header (fmt/data fehlt)." path))
    (let* ((bytes-per-frame (* channels (ceiling bits-per-sample 8)))
           (frames          (floor data-bytes bytes-per-frame)))
      (list :format          :wav
            :sample-rate      sample-rate
            :channels         channels
            :bits-per-sample  bits-per-sample
            :frames           frames
            :duration         (/ frames (coerce sample-rate 'double-float))))))

;;; ---------------------------------------------------------------------------
;;; AIFF / AIFC
;;; ---------------------------------------------------------------------------

(defun read-aiff-info (path stream)
  "Parst einen FORM/AIFF(-AIFC)-STREAM. Der FORM-Bezeichner ist bereits gelesen."
  (read-uint-be stream 4)                       ; Groesse des FORM-Chunks
  (let ((form (read-fourcc stream)))
    (unless (or (string= form "AIFF") (string= form "AIFC"))
      (error "~A ist keine AIFF-Datei (Form-Typ ~S)." path form)))
  (let (sample-rate channels bits-per-sample frames)
    (handler-case
        ;; Der COMM-Chunk liefert die Frame-Zahl direkt; der SSND-Audiodaten-
        ;; Chunk wird fuer die Dauer nicht benoetigt.
        (loop until (and sample-rate frames) do
          (let ((chunk-id   (read-fourcc stream))
                (chunk-size (read-uint-be stream 4)))
            (cond
              ((string= chunk-id "COMM")
               (setf channels        (read-uint-be stream 2)
                     frames          (read-uint-be stream 4)
                     bits-per-sample (read-uint-be stream 2)
                     sample-rate     (read-ieee-extended stream))
               (when (> chunk-size 18)          ; AIFC haengt Compression-ID an
                 (skip-bytes stream (- chunk-size 18))))
              (t
               (skip-bytes stream chunk-size)))
            (when (oddp chunk-size)
              (skip-bytes stream 1))))
      (end-of-file ()))
    (unless (and sample-rate frames)
      (error "~A: unvollstaendiger AIFF-Header (COMM fehlt)." path))
    (list :format          :aiff
          :sample-rate      sample-rate
          :channels         channels
          :bits-per-sample  bits-per-sample
          :frames           frames
          :duration         (/ frames sample-rate))))

;;; ---------------------------------------------------------------------------
;;; Oeffentliche Schnittstelle
;;; ---------------------------------------------------------------------------

(defun audio-file-info (path)
  "Liest den Header der WAV- oder AIFF-Datei unter PATH und liefert eine
   Property-List mit :FORMAT (:WAV oder :AIFF), :SAMPLE-RATE, :CHANNELS,
   :BITS-PER-SAMPLE, :FRAMES und :DURATION (Dauer in Sekunden)."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((magic (read-fourcc stream)))
      (cond
        ((string= magic "RIFF") (read-wav-info  path stream))
        ((string= magic "FORM") (read-aiff-info path stream))
        (t (error "~A: unbekanntes Audioformat (Kennung ~S)." path magic))))))

(defun audio-file-duration (path)
  "Liefert die Dauer der WAV- oder AIFF-Datei unter PATH in Sekunden."
  (getf (audio-file-info path) :duration))
