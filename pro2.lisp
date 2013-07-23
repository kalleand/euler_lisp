; Project Euler problem 2
;
; Finds the sum of all even fibonacci numbers under 4 million.

(defun pro2 (n1 n2)
  (cond
    ((> n2 4000000) 0)
    ((eq (mod n2 2) 0) (+ n2 (pro2 n2 (+ n1 n2))))
    (t (pro2 n2 (+ n1 n2)))))

(pro2 1 1)
