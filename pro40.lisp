#!/usr/bin/sbcl --script

(defun get-digits (num)
  (map 'list #'(lambda (c) (read-from-string (string c))) (prin1-to-string num)))

;;; Boring solution (uses a lot of memory).
;(time (let ((digits (cons 1 (loop for i from 1 to 200000 append (get-digits i)))))
  ;(* (nth 1 digits) (nth 10 digits) (nth 100 digits)
     ;(nth 1000 digits) (nth 10000 digits) (nth 100000 digits)
     ;(nth 1000000 digits))))

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

(format t "~D~%" (pro40 1 0 (list 1 10 100 1000 10000 100000 1000000)))
