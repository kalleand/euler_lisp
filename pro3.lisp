; Project Euler problem 3.
; 
; Finds the largest prime factor in 600851475143.

(defun pro3 (n i)
  (cond
    ((> i (sqrt n)) i)
    ((eq (mod n i) 0) (pro3 (/ n i) 2))
    (t (pro3 n (+ i 1)))))

(pro3 600851475143 2)
