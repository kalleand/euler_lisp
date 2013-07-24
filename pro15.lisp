#!/usr/bin/sbcl --script
; Project Euler problem 15
;
; Using the same memoization technique as in problem 14 we find the number of
; ways one can travel in a 20x20 field where you can only travel right or left.

; Memoization
(defun Basic-Memo (Function)
  (let ((Hash-Table (make-hash-table)))
    #'(lambda (Arg)
        (multiple-value-bind (Value Foundp)
          (gethash Arg Hash-Table)
          (if
            Foundp
            Value
            (setf (gethash Arg Hash-Table) (funcall Function Arg))))) ))

(defun Basic-Memoize (Function-Name)
  (setf (symbol-function Function-Name)
        (Basic-Memo (symbol-function Function-Name))))

; In the right and bottom edge the ways to move is 1 elsewhere it is number of
; ways to reach the goal if you go to the right plus the number of ways to reach 
; the goal if you go downwards.
(defun pro15 (n)
  (let ((a (floor (/ n 100)))
        (b (mod n 100)))
    (cond
      ((or (= 20 a) (= 20 b)) 1)
      (t (+ (pro15 (+ (* (+ a 1) 100) b)) (pro15 (+ (* a 100) (+ b 1))))))))

; Memoize the function pro15.
(Basic-Memoize 'pro15)

; Calculate the result.
(print (pro15 0))
