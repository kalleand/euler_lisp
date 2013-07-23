#!/usr/bin/sbcl --script
; Project Euler problem 6.
;
; Calculates the difference between the square of the sum and the sum of the
; square.
; 
; Done in two different ways. Second way is in my opinion much cooler as it uses
; a collector function (colpro6) to calculate the difference.

; Imperitive influenced way. (Boring way!)
;(defun sums (n)
  ;(/ (* n (+ n 1)) 2))

;(defun squares (n)
  ;(cond
    ;((eq n 1) 1)
    ;(t (+ (* n n) (squares (- n 1))))))

;(defun pro6 (n)
  ;(- (expt (sums n) 2) (squares n)))




; Much more exciting way that uses the collector function.
(defun colpro6 (a b)
  (- (* a a) b))

(defun pro6 (n col)
  (cond
    ((eq n 0) (funcall col 0 0))
    (t (pro6 (- n 1)
             (lambda (a b)
               (funcall col (+ a n) (+ (* n n) b)))))))

(print (pro6 100 (function colpro6)))
