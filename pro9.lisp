#!/usr/bin/sbcl --script

; Project Euler problem 9
;
; Finds the product of a, b and c
; where
; a < b < c
; a + b + c = 1000
; and
; a^2 + b^2 = c^2

(defun pro9 (b a)
  (cond
    ((= b 0) 0)
    ((= a 0) (pro9 (- b 1) (- b 2)))
    ((eq (+ (* a a) (* b b)) (expt (- (- 1000 a) b) 2)) (* a (* b (- (- 1000 a) b))))
    (t (pro9 b (- a 1)))))

(print (pro9 999 998))
