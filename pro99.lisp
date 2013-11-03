#!/usr/bin/sbcl --script

(load "~/.sbclrc")
(ql:quickload 'split-sequence)

(with-open-file (stream "base_exp.txt")
  (let ((max -1)
        (ln -1))
    (do ((line (read-line stream) (read-line stream nil nil))
         (linenumber 1 (1+ linenumber)))
      ((null line) (format t "~D~%" ln))
      (let* ((numbers (split-sequence:split-sequence #\, line))
             (n (* (log (read-from-string (first numbers)) 10) (read-from-string (second numbers)))))
        (if (> n max)
          (progn
            (setf max n)
            (setf ln linenumber)))))))
