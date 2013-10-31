#!/usr/bin/sbcl --script

;;; Works by first removing the first 10 digits of the fraction and then looping
;;; until you see the same fraction as the starting point.

(defun get-number-of-repeating-digits (n)
  (let ((start (mod (* (expt 10 10) (/ 1 n)) 1))
        (len 1))
    (do ((n (mod (* start 10) 1) (mod (* n 10) 1))) 
      ((= n start) len)
      (setf len (1+ len)))))


(let ((max 0)
      (highest 0))
  (loop for i from 2 to 1000 do
        (if (> (get-number-of-repeating-digits i) max)
          (progn
            (setf max (get-number-of-repeating-digits i))
            (setf highest i))))
  (format t "~D~%" highest))
