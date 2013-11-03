#!/usr/bin/sbcl --script

(defun get-digits (num)
  (map 'list #'(lambda (c) (read-from-string (string c))) (prin1-to-string num)))

(defun permuted? (n)
  (let ((digits (sort (get-digits n) #'<)))
    (every (lambda (li) (equal digits li))
           (loop for i from 2 to 6 collect (sort (get-digits (* i n)) #'<)))))

(loop for i = 1 then (1+ i)
      while (not (permuted? i))
      finally (format t "~D~%" i))
