#!/usr/bin/sbcl --script

(defun join-to-string (string-list)
    (format nil "~{~D~^~}" string-list))

(defun perm (lst)
  (flet ((con-lst (element perm-lst)
           (map 'list #'(lambda (l) (cons element l)) perm-lst)))
    (if (null lst)
      '(())
      (loop for e in lst
        append (con-lst e (perm (remove e lst)))))))

;;; The only way to achieve a pandigit in the sense described in the problem is
;;; to multiply a 1 digit long number with a 4 digit number or a 2 digit number
;;; with a 3 digit number
(defun pandigit? (lst)
  (if (or (= (* (read-from-string (join-to-string (subseq lst 0 1)))
                (read-from-string (join-to-string (subseq lst 1 5))))
             (read-from-string (join-to-string (subseq lst 5))))
          (= (* (read-from-string (join-to-string (subseq lst 0 2)))
                (read-from-string (join-to-string (subseq lst 2 5))))
             (read-from-string (join-to-string (subseq lst 5))))) t nil))

(format t "~D~%" (reduce #'+ (remove-duplicates 
                               (loop for i in (perm '(1 2 3 4 5 6 7 8 9))
                                     when (pandigit? i) collect
                                     (read-from-string (join-to-string (subseq i 5)))))))
