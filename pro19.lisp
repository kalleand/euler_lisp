#!/usr/bin/sbcl --script

; Project Euler problem 19.
;
; Calculates the number of months that starts with a monday between 1901 and
; 2001.

(defun sunday? (day)
  (cond
    ((= day 6) 1)
    (t 0)))

(defun calcday (orig days)
  (mod (+ orig days) 7))

(defun pro19 (day month year)
  (cond
    ((> year 2000) 0)
    ((= month 1)
     (cond
       ((and (zerop (mod year 4)) (> (mod year 400) 0)) (+ (sunday? day) (pro19 (calcday day 29) (+ month 1) year)))
       (t (+ (sunday? day) (pro19 (calcday day 28) (+ month 1) year)))))
    ((= month 11) (+ (sunday? day) (pro19 (calcday day 31) 0 (+ year 1))))
    ((or (= month 0) (= month 2) (= month 4) (= month 6) (= month 7) (= month 9)) (+ (sunday? day) (pro19 (calcday day 31) (+ month 1) year)))
    (t (+ (sunday? day) (pro19 (calcday day 30) (+ month 1) year)))))

(print (pro19 1 0 1901))
