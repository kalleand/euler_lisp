#!/usr/bin/sbcl --script

(defun get-digits (num)
  (map 'list #'(lambda (c) (read-from-string (string c))) (prin1-to-string num)))

;;; Boring solution (uses a lot of memory).
;;; I found the upper limit of 200000 through trial and error.
;(let ((digits (loop for i from 0 to 200000 append (get-digits i))))
;  (apply #'* (map 'list (lambda (a) (nth a digits)) (loop for x from 0 to 6 collect (expt 10 x)))))

;;; Better solution.
(defun pro40 (num order l)
  (cond
    ((null l) 1)
    (t (let* ((digits (get-digits num))
              (len (length digits)))
         (if (>= (+ order len) (car l))
           (* (nth (- len (- (+ order len) (car l)) 1) digits)
              (pro40 (1+ num) (+ order len) (cdr l)))
           (pro40 (1+ num) (+ order len) l))))))

(format t "~D~%" (pro40 1 0 (loop for x from 0 to 6 collect (expt 10 x))))
