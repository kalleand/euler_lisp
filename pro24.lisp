#!/usr/bin/sbcl --script
; Project Euler problem 24.
;
; The way this works is that it uses combinatorics and depend on the input to be
; an ordered list of numbers from 0 to a specific number (in this case 9).
;
; Then if n is over the number of permutations that can be achieved with the
; rest of the numbers available the current number is changed from the lowest
; possible.
;
; Special attention is made for when we want the first lexical permutation (it
; is the input) and the last lexical permutation (it is the reversed list).
; 
; The function is called with n as 999999 (one less than one million) because
; the list is 0 indexed.
; 
; The function that calculates the factorial could be made more efficiant with
; memoization. However, I feel this is not needed as it is called ten times in
; total during the calculation.
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
