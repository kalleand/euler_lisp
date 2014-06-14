#!/usr/bin/sbcl --script

(defun make-sieve (n)
  (make-array n :element-type 'bit :initial-element 0))

(defun mark (n sieve limit)
  (do ((i (* n n) (+ i n)))
    ((>= i limit) sieve)
    (setf (sbit sieve i) 1)))

(defun generate-primes (limit)
  (append (list 2)
          (let ((sieve (make-sieve limit)))
            (loop for i from 3 by 2 below limit when (zerop (sbit sieve i))
                  collect i and do (mark i sieve limit)))))

(defun prime? (n list-of-primes)
  (loop for p in list-of-primes
        while (<= (* 2 p) n)
        when (zerop (mod n p)) do (return-from prime? nil))
  t)

(defun get-sums (limit min-number-elem prime-list)
  (if (> min-number-elem (length prime-list)) nil
    (let ((min-sum (reduce #'+ (subseq prime-list 0 min-number-elem)))
          (rest-primes (subseq prime-list min-number-elem)))
      (loop for p in rest-primes
            for sum = (+ min-sum p) then (+ sum p)
            while (< sum limit)
            collect sum))))


(defun pro50 (limit)
  (let ((all-primes (generate-primes limit)))
    (loop for primes on all-primes
          with longest-sequence = 0
          with prime-sum = 0
          finally (return prime-sum) do
          (loop for i from longest-sequence
                for n in (get-sums limit longest-sequence primes)
                when (prime? n all-primes) do
                (progn (setf longest-sequence i)
                       (setf prime-sum n))))))

(format t "~d~%" (pro50 1000000))
