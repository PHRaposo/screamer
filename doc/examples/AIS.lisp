(IN-PACKAGE :SCREAMER)
    
 ;; ALL-INTERVAL SERIES PROBLEM 
    
(defun intervals-mod12v (list)
 (mapcar #'(lambda (x y)
  (funcallv #'mod (-v y x) 12))
  list (cdr list)))

(defun all-interval-seriesv ()
 (let* ((vars (append '(0)
	                 (n-variables 10 'a-member-ofv '(1 2 3 4 5 7 8 9 10 11))
		          '(6)))
        (intervalsv (intervals-mod12v vars)))
  (assert! (all-differentv vars))
  (assert! (all-differentv intervalsv))
  (list vars intervalsv)))

;;; (time (length (all-values (car (solution (all-interval-seriesv) (reorder #'domain-size #'(lambda (x) (declare (ignore x)) nil) #'< #'linear-force))))))
;;; > 3856

(defun apply-constraint (fun variables)
  (apply 
   (lambda (var)
     (if (apply fun (list var))
       var
       (fail)))
   (list variables))
  variables)

(defun alldifferent (l)
  (cond ((null l) t)
        ((member (car l) (cdr l) :test #'equal) nil)
        (t (alldifferent (cdr l)))))

(defun intervals-mod12 (list)
 (mapcar #'(lambda (x y)
  (mod (- y x) 12))
  list (cdr list)))
           
(defun all-interval-series-internal (&optional results)
 (cond ((= (length results) 11)
        (apply-constraint #'(lambda (variables)
                             (let ((intervals (intervals-mod12 variables)))
                              (and (alldifferent variables)
                                  (alldifferent intervals))))
                       (append results '(6))))
       ((null results)
        (all-interval-series-internal
         (apply-constraint #'alldifferent (cons 0 (list (a-member-of '(1 2 3 4 5 7 8 9 10 11)))))))
       (t (all-interval-series-internal 
           (apply-constraint #'(lambda (variables)
                                (let ((intervals (intervals-mod12 variables)))
                                 (and (alldifferent variables)
                                      (alldifferent intervals))))
                            (append results (list (a-member-of '(1 2 3 4 5 7 8 9 10 11)))))))))
    
(defun all-interval-series () (all-interval-series-internal))

;;; (time (length (all-values (all-interval-series))))
;;; > 3856
