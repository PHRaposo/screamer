(in-package :screamer)

(defun test-funcallv-ac-1 ()
  (let* ((*strategy* :ac)
         (x (an-integer-betweenv 0 2))
         (y (an-integer-betweenv 0 2))
         (z (funcallv #'list x y)))
   (assert! (equalv z '(1 2)))
   (known?-true (andv (=v x 1)
                      (=v y 2)))))

(defun test-funcallv-ac-2 ()
  (let* ((*strategy* :ac)
         (x (an-integer-betweenv 0 5))
         (y (an-integer-betweenv 0 5))
         (z (funcallv (lambda (a b) (+ (* a 2) (* b 3))) x y)))
    (assert! (=v z 11))
    (and (equal-set? (variable-enumerated-domain x) '(1 4))
         (equal-set? (variable-enumerated-domain y) '(1 3)))))
         
(defun test-funcallv-ac-3 ()
 (let* ((*strategy* :ac)
        (x (a-member-ofv '(0 1 2)))
        (y (a-member-ofv '(3 4 5)))
        (z (funcallv #'vector x y)))
  (assert! (memberv z '(#(1 4) #(1 5))))
  (known?-true (andv (=v x 1)
                     (known?-memberv y '(4 5))))))
                     
(defun test-funcallv-ac-4 ()
 (let* ((*strategy* :ac)
        (x (a-member-ofv '(0 1 2)))
        (y (a-member-ofv '(3 4 5)))
        (z (funcallv #'vector x y)))
  (assert! (equalv z #(1 4)))
  (known?-true (andv (=v x 1)
                     (=v y 4)))))

 (defun test-funcallv-ac-5 ()
 (let* ((*strategy* :ac)
        (x (a-member-ofv '(0 1 2)))
        (y (a-member-ofv '(3 4 5)))
        (z (funcallv #'vector x y)))
   (assert! (=v x 1))
   (assert! (=v y 5))
   (known?-equalv z #(1 5))))
  
(defun test-funcallv-bidirectional-propagation ()
  (let* ((x (an-integer-betweenv 0 2))
         (y (an-integer-betweenv 0 2))
         (z (funcallv #'+ x y)))
    (assert! (=v z 3))
    (assert! (=v x 1))
    (known?-true (=v y 2))))

(defun test-funcallv-bidirectional-propagation-2 ()
  (let* ((x (an-integer-betweenv 0 2))
         (y (an-integer-betweenv 0 2))
         (z (funcallv #'+ x y)))
    (assert! (=v z 3))
    (assert! (=v y 2))
    (known?-true (=v x 1))))

(defun test-funcallv-bidirectional-propagation-3 ()
  (let* ((x (an-integer-betweenv 0 2))
         (y (an-integer-betweenv 0 2))
         (z (funcallv #'+ x y)))
    (assert! (=v x 1))
    (assert! (=v y 2))
    (known?-true (=v z 3))))

(defun test-funcallv-lists-ac ()
 (let* ((*strategy* :ac)
        (x (list (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))))
        (z (funcallv #'remove-duplicates x)))
  (assert! (equalv z '(1)))
  (known?-equalv '(1 1 1 1 1) x)))

(defun test-funcallv-lists-gfc-1 ()
 (let* ((x (list (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))))
       (z (funcallv #'remove-duplicates x)))
  (assert! (equalv z '(1)))
  (equal-set? '(1 1 1 1 1)
   (one-value (solution x (static-ordering #'linear-force))))))

(defun test-funcallv-lists-gfc-2 ()
 (let* ((x (list (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))
                 (a-member-ofv '(0 1 2 3))))
       (z (funcallv #'remove-duplicates x)))
  (assert! (memberv z '((1) (2))))
  (equal-set? '((1 1 1 1 1) (2 2 2 2 2))
   (all-values (solution x (static-ordering #'linear-force))))))

(defparameter *funcallv-test-functions*
'(test-funcallv-ac-1
  test-funcallv-ac-2
  test-funcallv-ac-3
  test-funcallv-ac-4
  test-funcallv-ac-5  
  test-funcallv-bidirectional-propagation
  test-funcallv-bidirectional-propagation-2
  test-funcallv-bidirectional-propagation-3
  test-funcallv-lists-ac
  test-funcallv-lists-gfc-1
  test-funcallv-lists-gfc-2))

(cl:defun test-funcallv ()
  (let ((bug? nil))
    (flet ((run-test (fn)
            (let ((result
                    (handler-case
                        (funcall fn)
                      (error (e)
                        (format t "Error in ~A: ~A~%" fn e)
                        nil))))
              (unless result
                (format t "~%Test failed: ~A~%" fn)
                (setf bug? t)))))
      (dolist (test *funcallv-test-functions*)
        (run-test test))
      (if bug?
          (progn (format t "Screamer funcallv tests have a bug.~%") nil)
          (progn (format t "All funcallv tests passed.~%") t)))))
