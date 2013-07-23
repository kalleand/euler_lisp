#!/usr/bin/sbcl --script
; Project Euler problem 4
;
; Finds the largest palindromic number a and b where 
; 1 <= a < 1000 and 1 <= b < 1000
(defun pal (n)
  (cond
    ((and (and (= (mod n 10) (mod (floor (/ n 100000)) 10))
              (= (mod (floor (/ n 10)) 10) (mod (floor (/ n 10000)) 10)))
         (= (mod (floor (/ n 100)) 10) (mod (floor (/ n 1000)) 10))) t)
    (t nil)))

(defun pro4 (a b)
  (cond
    ((= a 0) 0)
    ((= b 0) (pro4 (- a 1) 999))
    ((pal (* a b)) (max (* a b) (pro4 (- a 1) 999)))
    (t (pro4 a (- b 1)))))

(print (pro4 999 999))
