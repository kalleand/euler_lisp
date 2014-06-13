#!/usr/bin/sbcl --script

(defun prime? (n list-of-primes)
  (loop for p in list-of-primes
        while (<= (* p 2) n)
        when (zerop (mod n p)) do (return-from prime? nil))
  t)

(defun generate-primes (upper-limit lower-limit)
  (loop for i from 3 by 2 below upper-limit
        with list-of-primes = '(2)
        when (prime? i list-of-primes) do (setf list-of-primes (append list-of-primes (list i)))
        finally  (return (remove-if
                           (lambda (n) (< n lower-limit))
                           list-of-primes))))

(defun get-digits (num)
  (map 'list (lambda (c) (read-from-string (string c))) (prin1-to-string num)))

(defun permutation? (first-item &rest rest-items)
  (loop for i in rest-items
        with perm = (sort (get-digits first-item) #'<)
        when (not (equal perm (sort (get-digits i) #'<))) do (return-from permutation? nil))
  t)

(defun arithmetic-sequence (n list-of-primes)
  (loop for p in list-of-primes
        for difference = (- p n)
        when (and (permutation? n p (+ p difference)) (member (+ p difference) list-of-primes))
        return (list n p (+ p difference))))

(loop for (p . rest) on (generate-primes 10000 1488) do
      (let ((seq (arithmetic-sequence p rest)))
        (if (not (null seq))
          (progn
            (format t "~d~d~d~%" (first seq) (second seq) (third seq))
            (return)))))
