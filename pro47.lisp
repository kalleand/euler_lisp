#!/usr/bin/sbcl --script

(defun get-prime-factors (n i)
  (cond
    ((= n 1) '())
    ((zerop (mod n 2)) (cons 2 (get-prime-factors (/ n 2) 3)))
    (t (let ((limit (truncate (sqrt n))))
         (loop for it from i to limit by 2
               if (zerop (mod n it)) do
               (return-from get-prime-factors (cons it (get-prime-factors (/ n it) it))))
         (return-from get-prime-factors (list n))))))

(loop for it from 1000
      with last = -1
      with consecutive = 0
      while (< consecutive 4)
      when (> (length (remove-duplicates (get-prime-factors it 3))) 3) do
      (if (and (> consecutive 0) (= (- it 1) last))
        (progn (incf consecutive)
               (incf last))
        (progn (setf consecutive 1)
               (setf last it)))
      finally (format t "~D~%" (- it 4)))

