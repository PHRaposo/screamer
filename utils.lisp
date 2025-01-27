;; VARIABLE GENERATORS

(in-package :screamer)

;; From Openmusic.
(cl:defun permut-random (input)
"Returns a random permutation of input."
 (labels
  ((takeout (i list)
    (cond ((= i 0) (subseq list 1 (length list)))
          ((= i (1- (length list))) (butlast list))
          (t (append
              (subseq list 0 i)
              (subseq list (1+ i) (length list)))))))
 (let ((list (copy-seq input))
      (r nil))
   (loop for i from 0 while (< i (length input)) do
        (unless (= 0 (length list))
          (let ((j (random (length list))))
            (push (elt list j) r)
            (setf list (takeout j list)))))
   r)))
 
;; nondeterministic 

(defun list-of-members-of (n dom)
  (if (zerop n) nil
      (cons (a-member-of dom)
            (list-of-members-of (1- n) dom))))

(defun list-of-random-members-of (n dom)
  (if (zerop n) nil
      (cons (a-random-member-of dom)
            (list-of-random-members-of (1- n) dom))))
						
(defun list-of-integers-between (n low high)
  (if (zerop n) nil
      (cons (an-integer-between low high)
            (list-of-integers-between (1- n) low high))))

;; constraint package
   
(defun a-random-member-ofv (values &optional (name nil name?))
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (memberv v (permut-random values)))
   (value-of v)))
   
(defun list-of-members-ofv (n dom)
  (if (zerop n) nil
      (cons (a-member-ofv dom)
            (list-of-members-ofv (1- n) dom))))

(defun list-of-integers-betweenv (n min max)
  (if (zerop n) nil
      (cons (an-integer-betweenv min max)
            (list-of-integers-betweenv (1- n) min max))))

(defun list-of-integers-abovev (n min)
  (if (zerop n) nil
      (cons (an-integer-abovev min)
            (list-of-integers-abovev (1- n) min))))

(defun list-of-integers-belowv (n max)
  (if (zerop n) nil
      (cons (an-integer-belowv max)
            (list-of-integers-belowv (1- n) max))))

(defun list-of-booleansv (n)
  (if (zerop n) nil
      (cons (a-booleanv)
            (list-of-booleansv (1- n)))))

(defun list-of-integersv (n)
  (if (zerop n) nil
      (cons (an-integerv)
            (list-of-integersv (1- n)))))

(defun list-of-realsv (n)
  (if (zerop n) nil
      (cons (a-realv)
            (list-of-realsv (1- n)))))

(defun list-of-reals-betweenv (n min max)
  (if (zerop n) nil
      (cons (a-real-betweenv min max)
            (list-of-reals-betweenv (1- n) min max))))

(defun list-of-reals-abovev (n min)
  (if (zerop n) nil
      (cons (a-real-abovev min)
            (list-of-reals-abovev (1- n) min))))

(defun list-of-reals-belowv (n max)
  (if (zerop n) nil
      (cons (a-real-belowv  max)
            (list-of-reals-belowv  (1- n) max))))

(defun list-of-numbersv (n)
  (if (zerop n) nil
      (cons (a-numberv)
            (list-of-numbersv (1- n)))))

(defun list-of-random-members-ofv (n dom)
  (if (zerop n) nil
      (cons (a-random-member-ofv dom)
            (list-of-random-members-ofv (1- n) dom))))

(defun list-of-lists-ofv (n-vars variables &optional args)
 (mapcar #'(lambda (x)
            (make-lists-ofv x variables args))
  n-vars))

(defun make-lists-ofv (n var &optional args)
 (cond
   ((string-equal var "an-integer-betweenv") (list-of-integers-betweenv n (first args) (second args)))
   ((string-equal var "a-member-ofv") (list-of-members-ofv n args))
   ((string-equal var "a-random-member-ofv") (list-of-random-members-ofv n args))
   ((string-equal var "a-booleanv") (list-of-booleansv n))
   ((string-equal var "an-integerv") (list-of-integersv n))
   ((string-equal var "an-integer-abovev") (list-of-integers-abovev n (first args)))
   ((string-equal var "an-integer-belowv") (list-of-integers-belowv n (first args)))
   ((string-equal var "a-realv") (list-of-realsv n))
   ((string-equal var "a-real-abovev")  (list-of-reals-abovev n (first args)))
   ((string-equal var "a-real-belowv") (list-of-reals-belowv n (first args)))
   ((string-equal var "a-real-betweenv") (list-of-reals-betweenv n (first args) (second args)))
   ((string-equal var "a-numberv") (list-of-numbersv n))))
 
;; constraints

(defun assert!-all-differentv (list)
;; Functionally the same as (apply #'/=v list) or (all-differentv list), but faster.
  (labels ((all-different (x xs)
           (if (null xs) nil
               (progn (assert!-notv-memberv x xs)
               (all-different (car xs) (cdr xs))))))
   (all-different (car list) (cdr list))))

	  
