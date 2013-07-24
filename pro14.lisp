#!/usr/bin/sbcl --script
; Project Euler problem 14
;
; Finds the largest collatz sequence where the starting integer is below
; one million.


; This is a memoization function. (Taken from
; http://www.csee.umbc.edu/courses/pub/Memoization/Memoization-1.0/Memoization-Examples.lisp)
(defun Basic-Memo (Function)
  "Takes a normal function object and returns an `equivalent' memoized one"
  (let ((Hash-Table (make-hash-table)))
    #'(lambda (Arg)
        (multiple-value-bind (Value Foundp)
          (gethash Arg Hash-Table)
          (if
            Foundp
            Value
            (setf (gethash Arg Hash-Table) (funcall Function Arg))))) ))

(defun Basic-Memoize (Function-Name)
  "Memoize function associated with Function-Name. Simplified version"
  (setf (symbol-function Function-Name)
        (Basic-Memo (symbol-function Function-Name))))

; Finds the next number in the collatz sequence.
(defun collatz (num)
  (cond
    ((= num 1) 1)
    ((= (mod num 2) 0) (+ 1 (collatz (/ num 2))))
    (t (+ 1 (collatz (+ (* 3 num) 1))))))

; Memoize the collatz function! This cuts execution time to 1/8 of the original
; (~9s => ~1.2s).
(Basic-Memoize 'collatz)

; Main function gets the number that produce the longest collatz sequence.
(defun pro14 (n i m)
  (cond
    ((zerop n) i)
    (t (let ((tmp (collatz n)))
         (cond
           ((> tmp m) (pro14 (- n 1) n tmp))
           (t (pro14 (- n 1) i m)))))))


(print (pro14 999999 0 0))
