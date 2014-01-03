#!/usr/bin/sbcl --script

; Project Euler problem 10.
;
; Finds the sum of all primes less than 2 million using sieve of Eratosthenes.

; Constructs the sieve.
(defun msieve (n)
  (make-array n :element-type 'bit :initial-element 0))

; Mark the multiples of n (starting from n^2).
(defun mark (n sieve)
  (do ((i (* n n) (+ i n)))
    ((> i 1999999) sieve)
    (setf (sbit sieve i) 1)))

; Checks every number up to 1999999 (it was less than 2 million).
(defun pro10 (num sieve col)
  (cond
    ((> num 1999999) col)
    ((zerop (sbit sieve num)) (pro10 (+ num 1) (mark num sieve) (+ num col)))
    (t (pro10 (+ num 1) sieve col))))

; Finds the sum.
(print (pro10 2 (msieve 2000000) 0))
