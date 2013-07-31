#!/usr/bin/sbcl --script
; Project Euler problem 25.
;
; Finds the first fibonacci number that has 1000 digits.

(defun pro25 (a b)
  (cond
    ((>= (/ b (expt 10 999)) 1) 1)
    (t (1+ (pro25 b (+ a b))))))

(print (pro25 0 1))
