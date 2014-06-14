#!/usr/bin/sbcl --script

(defun prime? (n list-of-primes)
  (loop for p in list-of-primes
        while (<= (* 2 p) n)
        when (zerop (mod n p)) do (return-from prime? nil))
  t)

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

(defun get-digits (num)
  (map 'list (lambda (c) (read-from-string (string c))) (prin1-to-string num)))

(defun apply-mask (mask lis n)
  (loop for m in (reverse mask)
        with i = -1
        if (zerop m) collect (nth (incf i) lis)
        else collect n))

(defun get-new-number (n mask replacement)
  (reduce #'+ (mapcar #'* (reverse (apply-mask mask (get-digits n) replacement))
                      (loop repeat 10 for i = 1 then (* i 10) collect i))))

(defun pro51 ()
  (let* ((primes (generate-primes 1000000))
         (masks '((1 1 0 0 0 1) (1 0 1 0 0 1) (1 0 0 1 0 1) (1 0 0 0 1 1)
                                (0 1 1 0 0 1) (0 1 0 1 0 1) (0 1 0 0 1 1)
                                (0 0 1 1 0 1) (0 0 1 0 1 1) (0 0 0 1 1 1))))
    (loop for n from 101 by 2 below 1000
          do (loop for mask in masks
                   when (< 7 (loop for i from 0 to 9
                                   with sum = 0 when
                                   (let ((new-num (get-new-number n mask i)))
                                     (if (< new-num 100000) nil (prime? new-num primes)))
                                   do (incf sum)
                                   finally (return sum)))
                   do (loop for i from 0 to 9 do
                            (if
                              (let ((new-num (get-new-number n mask i)))
                                (if (< new-num 100000) nil (prime? new-num primes)))
                              (progn (format t "~d~%" (get-new-number n mask i))
                                     (return-from pro51))))))))

(pro51)
