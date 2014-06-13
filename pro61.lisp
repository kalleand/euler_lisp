#!/usr/bin/sbcl --script

(defun generate-4-digit-numbers ()
  (loop for i from 1000 below 10000 collect i))

(defun square? (n) (zerop (mod (sqrt n) 1)))

(defun triangular? (n) (square? (1+ (* 8 n))))

(defun pentagonal? (n) (zerop (mod (/ (1+ (sqrt (1+ (* 24 n)))) 6) 1)))

(defun hexagonal? (n) (zerop (mod (/ (1+ (sqrt (1+ (* 8 n)))) 4) 1)))

(defun heptagonal? (n) (zerop (mod (/ (+ 3 (sqrt (+ 9 (* 40 n)))) 10) 1)))

(defun octagonal? (n) (zerop (mod (/ (1+ (sqrt (1+ (* 3 n)))) 3) 1)))

(defun add-if-not-nil (a b)
  (if (null a) nil (+ a b)))

(defun find-cyclic (input-list first-two last-two)
  (cond
    ((= 1 (length input-list))
     (find (+ (* last-two 100) first-two) (car input-list)))
    (t
     (find-if #'integerp
              (mapcar
                (lambda (sub-list)
                  (find-if #'integerp
                           (mapcar
                             (lambda (n)
                               (if (or (= -1 last-two) (= last-two (floor (/ n 100))))
                                 (add-if-not-nil
                                   (find-cyclic (remove sub-list input-list :test #'equal)
                                                (if (= -1 first-two)
                                                  (floor (/ n 100))
                                                  first-two)
                                                (mod n 100))
                                   n)))
                             sub-list)))
                input-list)))))

(defun pro61 ()
  (let* ((numbers (generate-4-digit-numbers))
         (filtered-numbers (list
                             (remove-if (complement #'triangular?) numbers)
                             (remove-if (complement #'square?) numbers)
                             (remove-if (complement #'pentagonal?) numbers)
                             (remove-if (complement #'hexagonal?) numbers)
                             (remove-if (complement #'heptagonal?) numbers)
                             (remove-if (complement #'octagonal?) numbers))))
    (find-cyclic filtered-numbers -1 -1)))

(format t "~d~%" (pro61))
