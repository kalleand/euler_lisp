#!/usr/bin/sbcl --script

; Project Euler problem 23.
;
; This is not as fast as I'd like it to be. It is correct however.
;
; The problem is to find the sum of all numbers that can not be expressed as a
; sum of two abundant numbers.

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

(defun abundant-sum (i l)
  (cond
    ((null l) i)
    ((not (null (member (- i (car l)) l))) 0)
    (t (abundant-sum i (cdr l)))))

(defun pro23 (i l)
  (cond
    ((> i 28123) 0)
    ((> (proper-divisors i) i) (+ (abundant-sum i l) (pro23 (+ i 1) (cons i l))))
    (t (+ (abundant-sum i l) (pro23 (+ i 1) l)))))

(print (pro23 0 '()))
