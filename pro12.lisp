#!/usr/bin/sbcl --script
; Project Euler problem 12.
;
; Finds the first triangular number that has over 500 divisors. (Bruteforce)

; The second conditional is to make sure the function is correct without it
; perfect squares (where (sqrt n) is a natural number) gets reported to have one
; extra divisor than they should.
(defun divisors (n i limit)
  (cond
    ((> i limit) 0)
    ((zerop (mod n i)) 
     (cond
       ((= limit i) 1)
       (t (+ (divisors n (+ i 1) limit) 2))))
    (t (divisors n (+ i 1) limit))))

(defun pro12 (n i)
  (cond
    ((> (divisors n 1 (floor (sqrt n))) 500) n)
    (t (pro12 (+ n i) (+ i 1)))))

(print (pro12 1 2))
