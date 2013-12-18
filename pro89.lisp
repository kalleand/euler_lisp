#!/usr/bin/sbcl --script
;;; Translation of roman numerals.
(defparameter *roman-numerals* '((#\M 1000) (#\D 500) (#\C 100) (#\L 50) (#\X 10) (#\V 5) (#\I 1)))
;;; You are not allowed to do IC for 99. The only numerals allowed to used are
;;; listed in this list (for example CM for 900 or IV for 4 are allowed).
(defparameter *preceding-numerals* '((1000 100) (500 100) (100 10) (50 10) (10 1) (5 1) (1 0)))

;;; Calculates the numerical value of the list of numeral representation of
;;; roman letters.
(defun calculate-roman-value (inlist)
  (cond
    ((null inlist) 0)
    ((null (second inlist)) (car inlist))
    ((< (car inlist) (second inlist))
     (+ (- (second inlist) (car inlist)) (calculate-roman-value (cdr (cdr inlist)))))
    (t (+ (car inlist) (calculate-roman-value (cdr inlist))))))

;;; Calculates the number of characters used to represent the target using roman
;;; numerals.
(defun optimal-characters (target numbers-list)
  (cond
    ((zerop target) 0)
    ((< target (car numbers-list))
     (let ((decr (loop for pair in *preceding-numerals*
                       when (= (car pair) (car numbers-list)) return (second pair))))
       (if (< target (- (car numbers-list) decr))
         (optimal-characters target (cdr numbers-list))
         (+ 2 (optimal-characters (- target (- (car numbers-list) decr)) (cdr numbers-list))))))
    (t (1+ (optimal-characters (- target (car numbers-list)) numbers-list)))))

;;; Calculates the number of saved characters if the optimal representation is
;;; used.
(defun saved-characters (input)
  (let ((numeral-value
          (map 'list
               (lambda (a) (loop for pair in *roman-numerals*
                                 when (char-equal (first pair) a) return (second pair)))
               (coerce input 'list))))
    (- (length input)
       (optimal-characters (calculate-roman-value numeral-value) (map 'list #'second *roman-numerals*)))))

;;; Reads every line of the file roman.txt and finds the number of potential
;;; saved characters using the optimal repersentation of the number.
(format t "~D~%"
        (with-open-file (stream "roman.txt")
          (loop for line = (read-line stream nil 'EOF) until (eq line 'EOF)
                sum (saved-characters line))))
