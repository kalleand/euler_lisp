#!/usr/bin/sbcl --script

;;; For this I tried to calculate if a number was pentagonal or hexagonal
;;; through its equations. However, for sufficiently large n values the sqrt
;;; function makes an error and its output can no longer be trusted. Hence this
;;; solution which is really quick as well.

(defun generate-hexagonal (n)
  (* n (- (* 2 n) 1)))

(defun generate-pentagonal (n)
  (/ (* n (- (* 3 n) 1)) 2))

(defun pro45 (&optional (peni 166) (hexi 144) (penn nil) (hexn nil))
  (let ((pn (or penn (generate-pentagonal peni)))
        (hn (or hexn (generate-hexagonal hexi))))
    (cond
      ((= pn hn) pn)
      ((> pn hn) (pro45 peni (1+ hexi) pn nil))
      (t (pro45 (1+ peni) hexi nil hn)))))

(format t "~D~%" (pro45))
