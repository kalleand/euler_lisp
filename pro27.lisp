#!/usr/bin/sbcl --script

(defun prime? (n)
  (cond
    ((< n 0) nil)
    ((= n 2) t)
    ((= (mod n 2) 0) 0)
    (t (let ((lim (sqrt n)))
       (do
         ((a 3 (+ a 2)))
         ((> a lim) t)
         (if (= (mod n a) 0) (return-from prime? nil)))))))

(defun get-number-primes (a b)
  (do
    ((n 0 (1+ n)))
    ((not (prime? (+ (* n n) (* a n) b))) n)))

(defun pro27 ()
  (let ((n-prime 0)
        (a-max 0)
        (b-max 0))
    (do ((n -999 (+ n 2)))
      ((> n 999) 'done)
      (do ((m -999 (+ m 2)))
        ((> m 999) nil)
        (let ((primes (get-number-primes n m)))
          (if (> primes n-prime)
            (progn
              (setf n-prime primes)
              (setf a-max n)
              (setf b-max m))))))
    (* a-max b-max)))

(format t "~D~%" (pro27))
