#!/usr/bin/sbcl --script

;;; Project Euler problem number 31.
;;;
;;; Get the number of ways we can get to 2 pounds using only coins.
;;; Expects li to be a list of the value of available coins sorted in
;;; descending order
(defun get-combinations (li sum target)
  (cond
    ((null li) 0)
    ((> sum target) 0)
    ((= sum target) 1)
    (t (+ (get-combinations li (+ sum (car li)) target)
          (get-combinations (cdr li) sum target)))))

(format t "~D~%" (get-combinations '(200 100 50 20 10 5 2 1) 0 200))
