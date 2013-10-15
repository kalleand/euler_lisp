#!/usr/bin/sbcl --script


(let ((table (make-hash-table)))
  (loop for a from 2 to 100 do
        (loop for b from 2 to 100 do
              (setf (gethash (expt a b) table) t)))
  (format t "~D~%" (hash-table-count table)))


