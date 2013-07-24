#!/usr/bin/sbcl --script

; Project Euler problem 16
;
; This one was really easy using LISP.
; Summing the digits in a large number (2^1000).
(defun pro16 (n)
  (cond
    ((zerop n) 0)
    (t (+ (mod n 10) (pro16 (floor (/ n 10)))))))

(print (pro16 (expt 2 1000)))
