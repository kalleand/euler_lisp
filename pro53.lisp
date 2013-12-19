#!/usr/bin/sbcl --script
(defparameter *fac-array* (make-array 101 :element-type 'number))

(loop for i from 1 to 100
      for a = 1 then (* a i) do
      (setf (aref *fac-array* i) a))

(defun take (n r)
  (/ (aref *fac-array* n) (* (aref *fac-array* r) (aref *fac-array* (- n r)))))

(format t "~D~&"
        (loop for i from 1 to 100 sum
              (loop for j from 1 below i
                    when (> (take i j) 1000000) sum 1)))
