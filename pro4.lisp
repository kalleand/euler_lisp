#!/usr/bin/sbcl --script
; Project Euler problem 4
;
; Finds the largest palindromic number a and b where 
; 1 <= a < 1000 and 1 <= b < 1000
(defun make-seq (n)
  (loop for i = n then (floor (/ i 10))
        while (not (zerop i))
        collect (mod i 10)))

(defun pal? (n) (let ((seq (make-seq n))) (equal seq (reverse seq))))

(defun pro4 (a b)
  (cond
    ((zerop a) 0)
    ((zerop b) (pro4 (- a 1) 999))
    ((pal? (* a b)) (max (* a b) (pro4 (- a 1) 999)))
    (t (pro4 a (- b 1)))))

(print (pro4 999 999))
