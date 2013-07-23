#!/usr/bin/sbcl --script
; Project Euler problem 7
;
; Finds the 10001st prime.
(defun prime? (n lat)
  (cond
    ((null lat) t)
    ((eq (mod n (car lat)) 0) nil)
    (t (prime? n (cdr lat)))))

(defun pro7 (n i lat)
  (cond
    ((prime? n lat) 
     (cond
       ((eq i 10001) n)
       (t (pro7 (+ n 2) (+ i 1) (cons n lat)))))
    (t (pro7 (+ n 2) i lat))))

(print (pro7 3 2 '(2)))
