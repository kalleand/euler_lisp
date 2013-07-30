#!/usr/bin/sbcl --script
; Project Euler problem 24.
(defun factorial (n)
  (cond
    ((zerop n) 1)
    (t (* n (factorial (1- n))))))

(defun pro24 (n p)
  (let ((f (factorial (length p))))
    (cond ((= n 0) p)
          ((= n f) (reverse p))
          ((> n f) (pro24 (mod n (* f (1+ (length p)))) p))
          (t (multiple-value-bind (d r) (floor n (/ f (length p)))
               (cons (nth d p) (pro24 r (remove (nth d p) p))))))))

(print (pro24 999999 '(0 1 2 3 4 5 6 7 8 9)))
