(in-package #:sound)

(defparameter sample-rate 48000)

(defun sine-wave (frequency duration &key (amplitude 1))
  "Generates a list of duration * sample-rate samples for a sine wave with the specified frequency"
  (loop
    with step = (/ (* 2 pi frequency)
                   sample-rate)
    for time from 0 to (1- (* duration sample-rate))
    for sample = (coerce (* amplitude (sin (+ (* step time))))
                         'single-float)
    collecting sample))

(defun int->bytes (int &optional (bytes 4))
  "Convert an integer into a little-endian list of its bytes"
  (loop for i from 0 to (1- bytes)
        collecting (ldb (byte 8 (* i 8)) int)))

(defun string->bytes (string)
  "Convert a string into a list of bytes by converting its characters to their ascii values"
  (loop for char across string
        collecting (char-code char)))

(defun float->bytes (float)
  "Convert a floating point number into a list of bytes by casting it to an int and calling int->bytes"
  (int->bytes (sb-kernel:single-float-bits float)))

(defun write-wave-file (sound filename)
  "Write sound into filename using the .wav file format. Format details available at https://www.mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html"
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
                           12                     ; fact chunk
                           (* 4 (length sound)))) ; each float is 4 bytes
            (string->bytes "WAVE")
            ;; Format block
            (string->bytes "fmt ")      ; block identifier
            (int->bytes 16)             ; block size
            (int->bytes 3 2)            ; audio format (IEEE floats)
            (int->bytes 1 2)            ; number of channels
            (int->bytes sample-rate)    ; sample rate
            (int->bytes (* 4 sample-rate)) ; bytes per second
            (int->bytes 4 2)               ; bytes per block
            (int->bytes 32 2)              ; bits per sample
            ;; Fact block
            (string->bytes "fact")
            (int->bytes 4)
            (int->bytes (length sound))
            ;; Data block
            (string->bytes "data")
            (int->bytes (* 4 (length sound))) ; size of data block
            (loop for sample in sound
                  nconcing (float->bytes sample)))
     file)))
