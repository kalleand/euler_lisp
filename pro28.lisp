#!/usr/bin/sbcl --script

(let ((n 1) (counter 0))
  (do ((side 3 (+ side 2)))
    ((> side 1001) nil)
    (loop repeat 4 do
          (setf n (+ n (- side 1)))
          (setf counter (+ counter n))))
  (format t "~D~%" (+ counter 1)))
