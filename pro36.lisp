#!/usr/bin/sbcl --script

(defun palindromic-helper (str)
  (let ((chars (loop for x across str collect x)))
    (equal chars (reverse chars))))

(defun palindromic? (n)
  (and (palindromic-helper (write-to-string n))
       (palindromic-helper (write-to-string n :base 2))))

(format t "~D~%" (loop for i from 1 to 1000000 when (palindromic? i) sum i))
