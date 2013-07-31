#!/usr/bin/sbcl --script
; Project Euler problem 25.
;
; Finds the first fibonacci number that has 1000 digits.
;
; We start at the second fibonacci number (1 ->1<- 2 3 5 8...).

(defun pro25 (a b)
  (cond
    ((>= (/ b (expt 10 999)) 1) 2)
    (t (1+ (pro25 b (+ a b))))))

(print (pro25 1 1))
