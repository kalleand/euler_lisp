#!/usr/bin/sbcl --script

(defun factorial (n)
  (cond 
    ((zerop n) 1)
    ((= 1 n) 1)
    (t (* n (factorial (- n 1))))))

(defun Basic-Memo (Function)
  (let ((Hash-Table (make-hash-table)))
    #'(lambda (Arg)
        (multiple-value-bind (Value Foundp)
          (gethash Arg Hash-Table)
          (if Foundp Value (setf (gethash Arg Hash-Table) (funcall Function Arg))))) ))

(defun Basic-Memoize (Function-Name)
  (setf (symbol-function Function-Name)
        (Basic-Memo (symbol-function Function-Name))))

(Basic-Memoize 'factorial)

(defun get-digits (num)
  (map 'list #'(lambda (c) (read-from-string (string c))) (prin1-to-string num)))

(defun factorial-sum (n)
  (reduce #'+ (map 'list (lambda (a) (factorial a)) (get-digits n))))

;;; 9!*7 is 2540160 which is a reasonable upper bound. However, these numbers
;;; we are looking for are called factorions and it exists two of them:
;;; 145 and 40585. We now use that as an upper bound to actually calculate
;;; anything. (The answer is 145 + 40585)
(format t "~D~%" (loop for i from 3 to 40585 when (= (factorial-sum i) i) sum i))
