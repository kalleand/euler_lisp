#!/usr/bin/sbcl --script
; Project Euler problem 5
;
; Finds the smallest integer evenly divisable by 1 through n).
; (takes way too much time after 29)
(defun divisable (n i)
  (cond
    ((eq i 1) t)
    ((eq (mod n i) 0) (divisable n (- i 1)))
    (t nil)))

(defun pro5 (n i)
  (cond
    ((divisable (* n i) n) (* n i))
    (t (pro5 n (+ i 1)))))

(print (pro5 20 1))
