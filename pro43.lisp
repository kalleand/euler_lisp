#!/usr/bin/sbcl --script

(defun perm (lst)
  (flet ((con-lst (element perm-lst)
           (map 'list #'(lambda (l) (cons element l)) perm-lst)))
    (if (null lst)
      '(())
      (loop for e in lst
        append (con-lst e (perm (remove e lst)))))))

(defun digits->int (lst)
  (reduce (lambda (x y) (+ y (* 10 x))) lst))

(defun divisibility? (n)
  (every (lambda (a) (= 0 (mod (digits->int (subseq n (first a) (+ (first a) 3)))
                               (second a))))
         '((0 2) (1 3) (2 5) (3 7) (4 11) (5 13) (6 17))))

;;; So first digit does not matter for divisibility and because the number of
;;; permutations of 10 digits are 10! which is 3628800 we divide it in 10 lists.
(format t "~D~%"
        (loop for i from 0 to 9 sum
              (loop for e in (perm (remove i '(0 1 2 3 4 5 6 7 8 9)))
                    when (divisibility? e) sum (digits->int (cons i e)))))
