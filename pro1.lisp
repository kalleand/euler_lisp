#!/usr/bin/sbcl --script
; Project Euler problem 1.
; 
; Sums the integers between 1 and 999 that are even divisable
; with either 3 or 5.
(defun pro1 (n)
        (cond 
                ((eq n 0) 0)
                ((or (eq (mod n 3) 0) (eq (mod n 5) 0)) (+ n (pro1 (- n 1))))
                (t (pro1 (- n 1)))))
(print (pro1 999))
