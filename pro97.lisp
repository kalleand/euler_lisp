#!/usr/bin/sbcl --script
(format t "~D~%" (mod (1+ (* 28433 (expt 2 7830457))) (expt 10 10)))
