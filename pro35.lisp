#!/usr/bin/sbcl --script

(defun make-sieve (n)
  (make-array n :element-type 'bit :initial-element 0))

(defun mark (n sieve)
  (do ((i (* n n) (+ i n)))
    ((> i 999999) sieve)
    (setf (sbit sieve i) 1)))

(defun sieve-helper (num sieve)
  (cond
    ((> num 999999) sieve)
    ((zerop (sbit sieve num)) (sieve-helper (+ num 1) (mark num sieve)))
    (t (sieve-helper (+ num 1) sieve))))

(defun get-sieve ()
  (sieve-helper 2 (make-sieve 1000000)))

(defun get-number (li)
  (let ((sum 0))
    (loop for i in li do
          (setf sum (+ i (* sum 10))))
    (return-from get-number sum))) 

(defun get-primes ()
  (let ((sieve (get-sieve)))
    (loop for it across sieve 
          for i from 0 to 1000000
          when (and (> i 1) (zerop it)) collect i)))

(defun get-ciruclar-numbers (num)
  (let ((size (- (length (prin1-to-string num)) 1))
        (res '())
        (n num))
    (loop repeat size do
          (setf n (+ (* (mod n 10) (expt 10 size)) (floor (/ n 10))))
          (setf res (cons n res)))
    (return-from get-ciruclar-numbers res)))

(defun all-primes? (li primes)
  (every #'(lambda (p) (member p primes)) li))

(let ((primes (get-primes)))
  (format t "~D~%" (length
                     (loop for p in (get-primes) 
                           when (all-primes? (get-ciruclar-numbers p) primes)
                           collect p))))
