#!/usr/bin/sbcl --script

(defun make-sieve (n)
  (make-array n :element-type 'bit :initial-element 0))

(defun mark (n sieve)
  (do ((i (* n n) (+ i n)))
    ((> i 7654321) sieve)
    (setf (sbit sieve i) 1)))

(defun sieve-helper (num sieve)
  (cond
    ((> num 7654321) sieve)
    ((zerop (sbit sieve num)) (sieve-helper (+ num 1) (mark num sieve)))
    (t (sieve-helper (+ num 1) sieve))))

(defun get-sieve ()
  (sieve-helper 2 (make-sieve 7654322)))

(defun get-primes ()
  (let ((sieve (get-sieve)))
    (loop for it across sieve 
          for i from 0 to 7654321
          when (and (> i 1) (zerop it)) collect i)))

(defun get-digits (num)
  (map 'list (lambda (c) (read-from-string (string c))) (prin1-to-string num)))

(defun pandigital? (n)
  (let* ((digits (get-digits n))
         (len (length digits)))
    (if (equalp (sort digits #'<) (subseq '(1 2 3 4 5 6 7 8 9) 0 len)) t nil)))

(format t "~D~%"
        (let ((primes (reverse (get-primes))))
          (loop for p in primes
                when (pandigital? p) do
                (return p))))
