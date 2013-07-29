#!/usr/bin/sbcl --script
; Project Euler problem 21.
; 
; Finds the sum of all the amicable numbers below 10000.
(defun proper-divisors (n)
 (proper-divisors-helper n 2 (sqrt n)))

(defun proper-divisors-helper (n i limit)
  (cond
    ((> i limit) 1)
    ((zerop (mod n i))
     (cond
       ((= limit i) (+ i 1))
       (t (+ (proper-divisors-helper n (+ i 1) limit) (+ i (/ n i))))))
    (t (proper-divisors-helper n (+ i 1) limit))))

(defun pro21 (n)
  (cond
    ((zerop n) 0)
    ((and (= n (proper-divisors (proper-divisors n ))) (not (= n (proper-divisors n)))) (+ n (pro21 (- n 1))))
    (t (pro21 (- n 1)))))

(print (pro21 9999))
