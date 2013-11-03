#!/usr/bin/sbcl --script 

(defun Basic-Memo (Function)
  (let ((Hash-Table (make-hash-table)))
    (lambda (Arg)
      (multiple-value-bind (Value Foundp)
        (gethash Arg Hash-Table)
        (if Foundp Value (setf (gethash Arg Hash-Table) (funcall Function Arg)))))))

(defun Basic-Memoize (Function-Name)
  (setf (symbol-function Function-Name)
        (Basic-Memo (symbol-function Function-Name))))

(defun prime? (n)
  (cond
    ((< n 2) nil)
    ((= n 2) t)
    ((zerop (mod n 2)) nil)
    (t (let ((lim (sqrt n)))
         (do
           ((a 3 (+ a 2)))
           ((> a lim) t)
           (if (= (mod n a) 0) (return-from prime? nil)))))))

(Basic-Memoize 'prime?)

(defun right-truncatable? (n)
  (do ((i (truncate (/ n 10)) (truncate (/ i 10))))
    ((= i 0) t)
    (if (not (prime? i)) (return-from right-truncatable? nil))))

(defun left-truncatable? (n)
  (let ((len (length (prin1-to-string n))))
    (every #'prime? (loop for i from (1- len) downto 1 collect (mod n (expt 10 i))))))

(defmacro truncatable-prime? (n)
  `(and (prime? ,n) (right-truncatable? ,n) (left-truncatable? ,n)))

(format t "~D~%"
        (loop for i = 11 then (1+ i)
              with counter = 0
              while (< counter 11)
              when (truncatable-prime? i) sum i
              and do (incf counter)))
