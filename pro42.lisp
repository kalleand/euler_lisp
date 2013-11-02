#!/usr/bin/sbcl --script

(load "~/.sbclrc")
(ql:quickload 'split-sequence)

(defun gen-triangle-numbers ()
  (loop for i from 1 to 40 collect (/ (* i (1+ i)) 2)))

(defun triangular? (n li)
  (not (null (member (loop for x across n sum (- (char-int x) 64)) li))))

(with-open-file (stream "words.txt")
  (let ((li (split-sequence:split-sequence #\, (remove #\" (read-line stream))))
        (triangle-numbers (gen-triangle-numbers))
        (tot 0))
    (loop for it in li when (triangular? it triangle-numbers) do
          (incf tot))
    (format t "~D~%" tot)))
