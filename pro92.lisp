#!/usr/bin/sbcl --script
(defparameter *bit-array* (make-array 568 :element-type 'bit))

(defun square (n) (* n n))

(defun get-next (num)
  (loop for a = num then (floor (/ a 10))
        until (zerop a)
        sum (square (mod a 10))))

(defun ends-in-89 (n)
  (aref *bit-array* (get-next n)))

(loop for x from 1 to 567 do
      (loop for a = x then (get-next a) do
            (if (= a 89) (progn (setf (aref *bit-array* x) 1) (return)))
            (if (= a 1) (return))))

(format t "~D~%" (loop for x from 1 below 10000000 sum (ends-in-89 x)))
