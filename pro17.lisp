#!/usr/bin/sbcl --script
; Project Euler problem 17.
;
; This was not a very nice program for LISP in my view. It got messy with a lot
; of special cases due to irregularities in the english language.
; 
; However, the problem is to count the number of letters used to count from one
; to one thousand.

(defun translate (n)
  (cond
    ((= (floor (/ n 1000)) 1) (string "onethousand"))
    ((>= (/ n 100) 1) 
     (cond
       ((= (mod n 100) 0) (concatenate 'string (translate (floor (/ n 100))) "hundred"))
       (t (concatenate 'string (translate (floor (/ n 100))) "hundredand" (translate (mod n 100))))))
    ((>= (/ n 10) 1)
     (cond
       ((= n 10) (string "ten"))
       ((= n 11) (string "eleven"))
       ((= n 12) (string "twelve"))
       ((= n 13) (string "thirteen"))
       ((= n 14) (string "fourteen"))
       ((= n 15) (string "fifteen"))
       ((= n 16) (string "sixteen"))
       ((= n 17) (string "seventeen"))
       ((= n 18) (string "eighteen"))
       ((= n 19) (string "nineteen"))
       (t (let ((tmp (floor (/ n 10))))
         (cond
           ((= tmp 2) (concatenate 'string "twenty" (translate (mod n 10))))
           ((= tmp 3) (concatenate 'string "thirty" (translate (mod n 10))))
           ((= tmp 4) (concatenate 'string "forty" (translate (mod n 10))))
           ((= tmp 5) (concatenate 'string "fifty" (translate (mod n 10))))
           ((= tmp 6) (concatenate 'string "sixty" (translate (mod n 10))))
           ((= tmp 7) (concatenate 'string "seventy" (translate (mod n 10))))
           ((= tmp 8) (concatenate 'string "eighty" (translate (mod n 10))))
           ((= tmp 9) (concatenate 'string "ninety" (translate (mod n 10)))))))))
    ((= n 0) (string ""))
    ((= n 1) (string "one"))
    ((= n 2) (string "two"))
    ((= n 3) (string "three"))
    ((= n 4) (string "four"))
    ((= n 5) (string "five"))
    ((= n 6) (string "six"))
    ((= n 7) (string "seven"))
    ((= n 8) (string "eight"))
    ((= n 9) (string "nine"))
    (t (string "SOMETHINGBROKE"))))

(defun pro17 (n)
  (cond
    ((zerop n) 0)
    (t (+ (length (translate n)) (pro17 (- n 1))))))

(print (pro17 1000))
