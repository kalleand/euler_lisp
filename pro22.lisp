#!/usr/bin/sbcl --script
; Project Euler problem 22.
; 
; In this assignment I somewhat cheated because sorting and manipulating the
; input into a list was done using bash and vim (I substituted every
; comma with a new line then I did a :%sort and then replaced the new
; line with a space and finally added a ( in the beginning and then a ) in the
; end). Also the file name should be names (not names.txt).

(defun get-score (str)
  (cond
    ((zerop (length str)) 0)
    (t (+ (- (char-code (char str 0)) 64) (get-score (subseq str 1 (length str)))))))

(defun pro22 (l i)
  (cond
    ((null l) 0)
    (t (+ (* i (get-score (car l))) (pro22 (cdr l) (+ i 1))))))

(defparameter *names* (open "names"))
(print (pro22 (read *names*) 1))
