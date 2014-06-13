#!/usr/bin/sbcl --script

(defun prime? (n list-of-primes)
  (loop for p in list-of-primes
        while (<= (* 2 p) n)
        when (zerop (mod n p)) do (return-from prime? nil))
  t)

(defun goldbach? (n list-of-primes)
  (loop for p in list-of-primes
        when (zerop (mod (sqrt (/ (- n p) 2)) 1)) return t))

(format t "~d~%"
        (loop for i from 3 by 2
              with primes = nil do
              (cond
                ((prime? i primes) (setf primes (append primes (list i))))
                ((not (goldbach? i primes)) (return i)))))
