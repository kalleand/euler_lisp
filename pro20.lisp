#!/usr/bin/sbcl --script

; Project Euler problem 20
;
; This one was really easy using LISP.
; Summing the digits in a large number (100!).
(defun pro20 (n)
  (cond
    ((zerop n) 0)
    (t (+ (mod n 10) (pro20 (floor (/ n 10)))))))

(defun factorial (n)
  (cond
    ((zerop n) 1)
    (t (* n (factorial (- n 1))))))

(print (pro20 (factorial 100)))
