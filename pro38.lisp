#!/usr/bin/sbcl --script

(defun get-digits (num)
  (map 'list #'(lambda (c) (read-from-string (string c)))
       (prin1-to-string num)))

(defun get-digits-str (str)
  (map 'list #'(lambda (c) (read-from-string (string c))) str))

(defun is-pandigit? (str match)
  (equalp (sort (get-digits-str str) #'<) match))

(defun get-pandigit (n match)
  (let ((str ""))
    (loop for i from 1
          while (< (length str) 9) do
          (setf str (concatenate 'string str (prin1-to-string (* n i))))
          finally
          (return (if (is-pandigit? str match) (read-from-string str) 0)))))

(let ((max 0)
      (match '(1 2 3 4 5 6 7 8 9)))
  (loop for i from 1 to 10000 do
        (let ((n (get-pandigit i match)))
          (if (> n max) (setf max n)))
        finally (format t "~D~%" max)))
