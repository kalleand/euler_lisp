#!/usr/bin/sbcl --script

(defun triangles (n) 
  (let ((counter 0))
    (loop for c from 3 to (truncate (/ n 2)) do
          (loop for b from 1 below (- n c) do
                (let ((a (- n c b)))
                  (if (and (> b a) (= (* c c) (+ (* b b) (* a a))))
                    (incf counter)))))
    (return-from triangles counter)))

(let ((p 1001)
      (max 0))
  (loop for i from 1000 downto 1 do
        (let ((n (triangles i)))
          (if (> n max)
            (progn (setf max n)
                   (setf p i)))))
  (format t "~D~%" p))
