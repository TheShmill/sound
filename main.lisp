(defpackage #:sound
  (:use :cl))

(in-package #:sound)

(defun int->bytes (int &optional (bytes 4))
  (loop for i from 0 to (1- bytes)
        collecting (ldb (byte 8 (* i 8)) int)))

(defun string->bytes (string)
  (loop for char across string
        collecting (char-code char)))

(defun float->bytes (float)
  (int->bytes (sb-kernel:single-float-bits float)))

(defparameter sample-rate 48000)

(defun wave-format-block ()
  (nconc (string->bytes "fmt ")         ; block identifier
         (int->bytes 16)                ; block size
         (int->bytes 3 2)               ; audio format (IEEE floats)
         (int->bytes 1 2)               ; number of channels
         (int->bytes sample-rate)       ; sample rate
         (int->bytes (* 4 sample-rate)) ; bytes per second
         (int->bytes 4 2)               ; bytes per block
         (int->bytes 32 2)))            ; bits per sample

(defun write-wave-file (sound filename)
  (with-open-file (file filename
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
    (write-sequence
     (nconc (string->bytes "RIFF")
            ;; file length
            (int->bytes (+ 4                      ; format id
                           24                     ; data format chunkt
                           8                      ; data header
                           12 ; fact chunk
                           (* 4 (length sound)))) ; each float is 4 bytes
            (string->bytes "WAVE") 
            (wav-format-block)
            (string->bytes "data")
            (int->bytes (* 4 (length sound))) ; size of data block
            ; fact block
            (string->bytes "fact")
            (int->bytes 4)
            (int->bytes (length sound))
            (loop for sample in sound
                  nconcing (float->bytes sample)))
     file)))

(defun sine-wave (freq duration)
  (loop
    with step = (/ (* 2 pi freq) sample-rate)
    for time from 0 to (1- (* duration sample-rate))
    for sample = (coerce (sin (* step time))
                         'single-float)
        collecting sample))
