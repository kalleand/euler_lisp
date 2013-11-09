#!/usr/bin/sbcl --script

(defun pentagon (n)
  (/ (* n (- (* 3 n) 1)) 2))

(defun pentagon? (y)
  (= 0 (mod (+ (sqrt (+ (* 2/3 y) 1/36)) 1/6) 1)))

(defun pro44 ()
  (loop for i from 10
        for bp = (pentagon i) do
        (loop for j from (- i 1) downto 1
              for lp = (pentagon j)
              when (and (pentagon? (+ bp lp))
                        (pentagon? (- bp lp)))
              do (return-from pro44 (- bp lp)))))

(format t "~D~%" (pro44))
