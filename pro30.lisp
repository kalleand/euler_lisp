#!/usr/bin/sbcl --script

(defun get-digits (num)
  (map 'list #'(lambda (c) (read-from-string (string c))) (prin1-to-string num)))

(defun digit-fifth-power? (num)
  (= num (reduce #'+ (map 'list #'(lambda (n) (expt n 5)) (get-digits num)))))

;;; This magic number that is 999999 is a result of me noticing that the the
;;; numbers need to be less than 7 digits (9^5 * 7 has 6 digits).
(format t "~D~%" (loop for n from 2 to 999999 when (digit-fifth-power? n) sum n))
