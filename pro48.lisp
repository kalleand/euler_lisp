#!/usr/bin/sbcl --script

(format t "~D~%" (mod (loop for i from 1 to 1000 sum (expt i i)) (expt 10 10)))
