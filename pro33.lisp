#!/usr/bin/sbcl --script 

(defun get-candidates (n)
  (let ((sum '()))
    (loop for j from 1 to 9 do
          (loop for i from 1 below j do
                (let ((frac (/ i j)))
                  (if (equalp frac (/ (+ n (* i 10)) (+ n (* j 10))))
                    (setf sum (cons frac sum)))     
                  (if (equalp frac (/ (+ i (* n 10)) (+ n (* j 10))))
                    (setf sum (cons frac sum)))     
                  (if (equalp frac (/ (+ n (* i 10)) (+ j (* n 10))))
                    (setf sum (cons frac sum)))     
                  (if (equalp frac (/ (+ i (* n 10)) (+ j (* n 10))))
                    (setf sum (cons frac sum)))))
          finally (return sum))))

(format t "~D~%"
        (reduce #'* (loop for it from 1 to 9 append (get-candidates it))))
