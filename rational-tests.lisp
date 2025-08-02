(in-package :screamer)

#+lispworks(setf sys::*stack-overflow-behaviour* :warn)

(defun equal-set? (x y)
 (and (subsetp x y :test #'equal) (subsetp y x :test #'equal)))

;;; Find a ratio (non-integer rational) above 0 with denominator <= 1000, close to sqrt(2).
(defun test-a-ratio-above-sqrt2 ()
  "Returns a ratio above 0, denominator <= 1000, within 1/1000 of sqrt(2)."
  (= 1262/893 
    (one-value
    (let ((x (a-ratio-above 0 1000)))
      (unless (< (abs (- x (sqrt 2))) 1/1000)
        (fail))
      x))))

;;; Find a rational between 2 and 3 with denominator <= 1000, close to e.
(defun test-a-rational-between-e ()
  "Returns a rational in [2, 3], denominator <= 1000, within 1/1000 of Euler's number e."
  (= 2201/810
   (one-value
    (let ((x (a-rational-between 2 3 1000)))
      (unless (< (abs (- x (exp 1))) 1/1000)
        (fail))
      x))))

;;; Find a ratio below 1 with denominator <= 1000, close to ln(2).
(defun test-a-ratio-below-ln2 ()
  "Returns a ratio below 1, denominator <= 1000, within 1/1000 of ln(2)."
  (= 581/837
  (one-value
    (let ((x (a-ratio-below 1 1000)))
      (unless (< (abs (- x (log 2))) 1/1000)
        (fail))
      x))))

;;; Find a rational above 1 with denominator <= 1000, close to the golden ratio.
(defun test-a-rational-above-golden-ratio ()
  "Returns a rational above 1, denominator <= 1000, within 1/1000 of the golden ratio phi."
  (= 1575/974
   (one-value
    (let ((phi (/ (+ 1 (sqrt 5)) 2))
          (x (a-rational-above 1 1000)))
      (unless (< (abs (- x phi)) 1/1000)
        (fail))
      x))))

;;; Find a ratio between 1.5 and 1.7 with denominator <= 1000, close to pi/2.
(defun test-a-ratio-between-pi-halves ()
  "Returns a ratio in [1.5, 1.7], denominator <= 1000, within 1/1000 of pi/2."
  (= 1237/788
   (one-value
    (let ((x (a-ratio-between 1.5 1.7 1000)))
      (unless (< (abs (- x (/ pi 2))) 1/1000)
        (fail))
      x))))

;; Integer domain
(defun test-integer-domain ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3 1.0 2.0 3.0)))
    (assert! (integerpv x))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1 2 3))))

;; Rational domain intersection and pruning
(defun test-rational-domain ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3 1.0 2.0 3.0)))
    (assert! (rationalpv x))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4 1 2 3))))

;; Ratio domain intersection and pruning
(defun test-noninteger-domain ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3 1.0 2.0 3.0)))
    (assert! (notv (integerpv x)))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1.0 2.0 3.0 1/2 2/3 3/4))))

;; Rational domain with real constraint
(defun test-rational-real ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3 1.0 2.0 3.0)))
    (assert! (realpv x))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4 1 2 3 1.0 2.0 3.0))))

;; Ratio domain with rational constraint and pruning
(defun test-ratio-rational-prune ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3)))
    (assert! (andv (rationalpv x) (notv (integerpv x))))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4))))

;; Known? for ratio type
(defun test-known-ratiopv.1 ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4)))
    (assert! (notv (integerpv x)))
    (known?-ratiopv x)))

(defun test-known-ratiopv.2 ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1.0 2.0 3.0 1/2 2/3 3/4 1 2 3)))
    (assert! (ratiopv x))
    (known?-ratiopv x)))

(defun test-known-notv-ratiopv ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1.0 2.0 3.0 1/2 2/3 3/4 1 2 3)))
    (assert! (notv (ratiopv x)))
    (known?-notv-ratiopv x)))

;; Known? for rational type
(defun test-known-rationalpv ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3)))
    (assert! (rationalpv x))
    (known?-rationalpv x)))

(defun test-known-notv-rationalpv ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1.0 2.0 3.0 1/2 2/3 3/4 1 2 3)))
    (assert! (notv (rationalpv x)))
    (known?-notv-rationalpv x)))

;; Pruning with mixed types
(defun test-mixed-type-prune ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3 a b c)))
    (assert! (numberpv x))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4 1 2 3))))

(defun test-mixed-type-ratio ()
  (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1 2 3 a b c)))
    (assert! (andv (numberpv x) (notv (integerpv x))))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4))))

;; Ratio domain with explicit denominator bound
(defun test-ratio-denominator-bound.1 ()
 (let ((x (make-variable)))
    (assert! (memberv x '(1/2 2/3 3/4 1/5 2/7 1 2 3)))
    (assert! (andv (rationalpv x) (notv (integerpv x))))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4 1/5 2/7))))

;; Ratio domain with explicit denominator bound
(defun test-ratio-denominator-bound.2 ()
  (let ((x (a-ratio-betweenv 0 1 4)))
    ;; Only ratios with denominator <= 4 using variable slot (max-denom)
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/4 1/3 1/2 2/3 3/4))))

(defun test-ratio-denominator-bound.3 ()
  (let ((x (a-ratio-betweenv 0 1 4))
        (y (a-ratio-betweenv 0 1 6)))
    ;; x: only ratios with denominator <= 4
    ;; y: only ratios with denominator <= 6
  (equal-set?
   (all-values (solution (list x y) (static-ordering #'linear-force)))
'((1/4 1/6) (1/4 1/5) (1/4 1/4) (1/4 1/3) (1/4 2/5) (1/4 1/2) (1/4 3/5) (1/4 2/3) (1/4 3/4) (1/4 4/5) (1/4 5/6)
  (1/3 1/6) (1/3 1/5) (1/3 1/4) (1/3 1/3) (1/3 2/5) (1/3 1/2) (1/3 3/5) (1/3 2/3) (1/3 3/4) (1/3 4/5) (1/3 5/6)
  (1/2 1/6) (1/2 1/5) (1/2 1/4) (1/2 1/3) (1/2 2/5) (1/2 1/2) (1/2 3/5) (1/2 2/3) (1/2 3/4) (1/2 4/5) (1/2 5/6)
  (2/3 1/6) (2/3 1/5) (2/3 1/4) (2/3 1/3) (2/3 2/5) (2/3 1/2) (2/3 3/5) (2/3 2/3) (2/3 3/4) (2/3 4/5) (2/3 5/6)
  (3/4 1/6) (3/4 1/5) (3/4 1/4) (3/4 1/3) (3/4 2/5) (3/4 1/2) (3/4 3/5) (3/4 2/3) (3/4 3/4) (3/4 4/5) (3/4 5/6)))))

  (defun test-propagation-integer-plus-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (integerpv x))
      (assert! (integerpv y))
      (known?-integerpv z)))

  (defun test-propagation-rational-plus-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (rationalpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-plus-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (ratiopv x))
      (assert! (integerpv y))
      (known?-ratiopv z)))

  (defun test-propagation-integer-plus-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (ratiopv y))
      (assert! (integerpv x))
      (known?-ratiopv z)))

  (defun test-propagation-real-plus-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (known?-realpv z)))

  (defun test-propagation-float-plus-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (notv (rationalpv x)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-real-plus-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-rational-plus-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (rationalpv x))
      (assert! (integerpv y))
      (known?-rationalpv z)))

  (defun test-propagation-integer-plus-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (integerpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-plus-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (ratiopv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-float-plus-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (+v x y)))
      (assert! (notv (rationalpv x)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-integer-times-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (integerpv x))
      (assert! (integerpv y))
      (known?-integerpv z)))

  (defun test-propagation-rational-times-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (rationalpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-times-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (ratiopv x))
      (assert! (integerpv y))
      (known?-rationalpv z)))

  (defun test-propagation-integer-times-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (integerpv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-real-times-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (known?-realpv z)))

  (defun test-propagation-float-times-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (notv (rationalpv x)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-real-times-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-rational-times-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (rationalpv x))
      (assert! (integerpv y))
      (known?-rationalpv z)))

  (defun test-propagation-integer-times-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (integerpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-times-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (ratiopv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-float-times-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (*v x y)))
      (assert! (notv (rationalpv x)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-integer-minus-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (integerpv x))
      (assert! (integerpv y))
      (known?-integerpv z)))

  (defun test-propagation-rational-minus-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (rationalpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-minus-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (ratiopv x))
      (assert! (integerpv y))
      (known?-ratiopv z)))

  (defun test-propagation-integer-minus-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (integerpv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-float-minus-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (notv (rationalpv x)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-real-minus-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-real-minus-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (known?-realpv z)))

  (defun test-propagation-rational-minus-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (rationalpv x))
      (assert! (integerpv y))
      (known?-rationalpv z)))

  (defun test-propagation-integer-minus-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (integerpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-minus-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (-v x y)))
      (assert! (ratiopv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-float-minus-float ()
    (let* ((x (a-member-ofv '(1.0 1.25 1.5 1.75 2.0)))
           (y (a-member-ofv '(0.5 0.75 1.0 1.25 1.5)))
           (z (-v x y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-integer-div-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (integerpv x))
      (assert! (integerpv y))
      (known?-rationalpv z)))

  (defun test-propagation-rational-div-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (rationalpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-div-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (ratiopv x))
      (assert! (integerpv y))
      (known?-ratiopv z)))

  (defun test-propagation-integer-div-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (integerpv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-real-div-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (known?-realpv z)))

  (defun test-propagation-float-div-real ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (notv (rationalpv x)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-real-div-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

  (defun test-propagation-rational-div-integer ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (rationalpv x))
      (assert! (integerpv y))
      (known?-rationalpv z)))

  (defun test-propagation-integer-div-rational ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (integerpv x))
      (assert! (rationalpv y))
      (known?-rationalpv z)))

  (defun test-propagation-ratio-div-ratio ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (ratiopv x))
      (assert! (ratiopv y))
      (known?-rationalpv z)))

  (defun test-propagation-float-div-float ()
    (let* ((x (a-realv))
           (y (a-realv))
           (z (/v x y)))
      (assert! (notv (rationalpv x)))
      (assert! (notv (rationalpv y)))
      (known?-notv-rationalpv z)))

(defun test-propagation-max-denom.integer-ratio-div.1 ()
  (let* ((x (an-integer-betweenv 0 5))
         (y (a-ratio-betweenv 0 2 4))
         (z (/v x y)))
    (= (variable-max-denom z) 7)))

(defun test-propagation-max-denom.integer-ratio-div.2 ()
  (let* ((x (an-integer-betweenv 0 10))
         (y (a-ratio-betweenv 0 3 8))
         (z (/v x y)))
    (= (variable-max-denom z) 23)))

(defun test-propagation-max-denom.ratio-integer-div.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (an-integer-betweenv 0 5))
         (z (/v x y)))
    (= (variable-max-denom z) 16)))

(defun test-propagation-max-denom.ratio-integer-div.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (an-integer-betweenv 0 10))
         (z (/v x y)))
    (= (variable-max-denom z) 72)))

(defun test-propagation-max-denom.integer-rational-div.1 ()
  (let* ((x (an-integer-betweenv 0 5))
         (y (a-rational-betweenv 0 2 4))
         (z (/v x y)))
    (= (variable-max-denom z) 7)))

(defun test-propagation-max-denom.integer-rational-div.2 ()
  (let* ((x (an-integer-betweenv 0 10))
         (y (a-rational-betweenv 0 3 8))
         (z (/v x y)))
    (= (variable-max-denom z) 23)))

(defun test-propagation-max-denom.rational-ratio-div.1 ()
  (let* ((x (a-rational-betweenv 0 2 4))
         (y (a-ratio-betweenv 0 2 4))
         (z (/v x y)))
    (= (variable-max-denom z) 28)))

(defun test-propagation-max-denom.rational-ratio-div.2 ()
  (let* ((x (a-rational-betweenv 0 3 8))
         (y (a-ratio-betweenv 0 3 8))
         (z (/v x y)))
    (= (variable-max-denom z) 184)))

(defun test-propagation-max-denom.ratio-rational-div.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (a-rational-betweenv 0 2 4))
         (z (/v x y)))
    (= (variable-max-denom z) 28)))

(defun test-propagation-max-denom.ratio-rational-div.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (a-rational-betweenv 0 3 8))
         (z (/v x y)))
    (= (variable-max-denom z) 184)))

(defun test-propagation-max-denom.integer-ratio-plus.1 ()
  (let* ((x (an-integer-betweenv 0 5))
         (y (a-ratio-betweenv 0 2 4))
         (z (+v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.integer-ratio-plus.2 ()
  (let* ((x (an-integer-betweenv 0 10))
         (y (a-ratio-betweenv 0 3 8))
         (z (+v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.ratio-integer-plus.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (an-integer-betweenv 0 5))
         (z (+v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.ratio-integer-plus.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (an-integer-betweenv 0 10))
         (z (+v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.rational-ratio-plus.1 ()
  (let* ((x (a-rational-betweenv 0 2 4))
         (y (a-ratio-betweenv 0 2 4))
         (z (+v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.rational-ratio-plus.2 ()
  (let* ((x (a-rational-betweenv 0 3 8))
         (y (a-ratio-betweenv 0 3 8))
         (z (+v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.ratio-rational-plus.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (a-rational-betweenv 0 2 4))
         (z (+v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.ratio-rational-plus.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (a-rational-betweenv 0 3 8))
         (z (+v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.integer-ratio-times.1 ()
  (let* ((x (an-integer-betweenv 0 5))
         (y (a-ratio-betweenv 0 2 4))
         (z (*v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.integer-ratio-times.2 ()
  (let* ((x (an-integer-betweenv 0 10))
         (y (a-ratio-betweenv 0 3 8))
         (z (*v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.ratio-integer-times.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (an-integer-betweenv 0 5))
         (z (*v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.ratio-integer-times.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (an-integer-betweenv 0 10))
         (z (*v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.rational-ratio-times.1 ()
  (let* ((x (a-rational-betweenv 0 2 4))
         (y (a-ratio-betweenv 0 2 4))
         (z (*v x y)))
    (= (variable-max-denom z) 16)))

(defun test-propagation-max-denom.rational-ratio-times.2 ()
  (let* ((x (a-rational-betweenv 0 3 8))
         (y (a-ratio-betweenv 0 3 8))
         (z (*v x y)))
    (= (variable-max-denom z) 64)))

(defun test-propagation-max-denom.ratio-rational-times.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (a-rational-betweenv 0 2 4))
         (z (*v x y)))
    (= (variable-max-denom z) 16)))

(defun test-propagation-max-denom.ratio-rational-times.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (a-rational-betweenv 0 3 8))
         (z (*v x y)))
    (= (variable-max-denom z) 64)))

(defun test-propagation-max-denom.integer-ratio-minus.1 ()
  (let* ((x (an-integer-betweenv 0 5))
         (y (a-ratio-betweenv 0 2 4))
         (z (-v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.integer-ratio-minus.2 ()
  (let* ((x (an-integer-betweenv 0 10))
         (y (a-ratio-betweenv 0 3 8))
         (z (-v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.ratio-integer-minus.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (an-integer-betweenv 0 5))
         (z (-v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.ratio-integer-minus.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (an-integer-betweenv 0 10))
         (z (-v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.rational-ratio-minus.1 ()
  (let* ((x (a-rational-betweenv 0 2 4))
         (y (a-ratio-betweenv 0 2 4))
         (z (-v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.rational-ratio-minus.2 ()
  (let* ((x (a-rational-betweenv 0 3 8))
         (y (a-ratio-betweenv 0 3 8))
         (z (-v x y)))
    (= (variable-max-denom z) 8)))

(defun test-propagation-max-denom.ratio-rational-minus.1 ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (a-rational-betweenv 0 2 4))
         (z (-v x y)))
    (= (variable-max-denom z) 4)))

(defun test-propagation-max-denom.ratio-rational-minus.2 ()
  (let* ((x (a-ratio-betweenv 0 3 8))
         (y (a-rational-betweenv 0 3 8))
         (z (-v x y)))
    (= (variable-max-denom z) 8)))

(defun test-maxv-max-denom ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (a-ratio-betweenv 1 2 6))
         (z (maxv x y)))
    (and (= (variable-max-denom z) 6)
         (every (lambda (sol) (<= (denominator (third sol)) 6))
                (all-values (solution (list x y z) (static-ordering #'linear-force)))))))

(defun test-minv-max-denom ()
  (let* ((x (a-ratio-betweenv 0 2 4))
         (y (a-ratio-betweenv 1 2 6))
         (z (minv x y)))
    (and (= (variable-max-denom z) 6)
         (every (lambda (sol) (<= (denominator (third sol)) 6))
                (all-values (solution (list x y z) (static-ordering #'linear-force)))))))

(defun test-floatpv ()
  (let ((x (a-member-ofv '(1.0 2.0 3.0 1/2 2/3 3/4 1 2 3))))
    (assert! (floatpv x))
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1.0 2.0 3.0))))

(defun test-notv-floatpv ()
  (let ((x (a-member-ofv '(1.0 2.0 3.0 1/2 2/3 3/4 1 2 3))))
    (assert! (notv (floatpv x)))  
    (equal-set? (all-values (solution x (static-ordering #'linear-force)))
                '(1/2 2/3 3/4 1 2 3))))

(defun test-share-equalv-rational-enum-max-denom ()
  (let* ((x (a-rationalv 4))
         (y (a-rationalv 6)))
    (assert! (memberv x '(1/2 2/3 3/4 1 5/4 3/2 2)))
    (assert! (memberv y '(2/3 3/4 1 5/6 7/6 3/2 2)))
    ;; Unify
    (assert! (equalv x y))
    (let ((expected '(2/3 3/4 1 3/2 2)))
      (and
        (equal-set? (all-values (solution x (static-ordering #'linear-force))) expected)
        (equal-set? (all-values (solution y (static-ordering #'linear-force))) expected)
        (= (variable-max-denom x) 4)
        (= (variable-max-denom y) 4)))))

(defparameter *rational-ordeal-tests*
  '(test-a-ratio-above-sqrt2
    test-a-rational-between-e
    test-a-ratio-below-ln2
    test-a-rational-above-golden-ratio
    test-a-ratio-between-pi-halves
    test-integer-domain
    test-rational-domain
    test-noninteger-domain
    test-rational-real
    test-ratio-rational-prune
    test-known-ratiopv.1
    test-known-ratiopv.2
    test-known-notv-ratiopv
    test-known-rationalpv
    test-known-notv-rationalpv
    test-mixed-type-prune
    test-mixed-type-ratio
    test-ratio-denominator-bound.1
    test-ratio-denominator-bound.2
    test-ratio-denominator-bound.3
    test-propagation-integer-plus-integer
    test-propagation-rational-plus-rational
    test-propagation-ratio-plus-integer
    test-propagation-integer-plus-ratio
    test-propagation-real-plus-real
    test-propagation-float-plus-real
    test-propagation-rational-plus-integer
    test-propagation-integer-plus-rational
    test-propagation-ratio-plus-ratio
    test-propagation-float-plus-float
    test-propagation-integer-times-integer
    test-propagation-rational-times-rational
    test-propagation-ratio-times-integer
    test-propagation-real-times-real
    test-propagation-float-times-real
    test-propagation-rational-times-integer
    test-propagation-integer-times-rational
    test-propagation-ratio-times-ratio
    test-propagation-float-times-float
    test-propagation-integer-minus-integer
    test-propagation-rational-minus-rational
    test-propagation-ratio-minus-integer
    test-propagation-float-minus-real
    test-propagation-real-minus-real
    test-propagation-rational-minus-integer
    test-propagation-integer-minus-rational
    test-propagation-ratio-minus-ratio
    test-propagation-float-minus-float
    test-propagation-integer-div-integer
    test-propagation-rational-div-rational
    test-propagation-ratio-div-integer
    test-propagation-integer-div-ratio
    test-propagation-real-div-real
    test-propagation-float-div-real
    test-propagation-rational-div-integer
    test-propagation-integer-div-rational
    test-propagation-ratio-div-ratio
    test-propagation-float-div-float
    test-propagation-max-denom.integer-ratio-div.1
    test-propagation-max-denom.integer-ratio-div.2
    test-propagation-max-denom.ratio-integer-div.1
    test-propagation-max-denom.ratio-integer-div.2
    test-propagation-max-denom.integer-rational-div.1
    test-propagation-max-denom.integer-rational-div.2
    test-propagation-max-denom.rational-ratio-div.1
    test-propagation-max-denom.rational-ratio-div.2
    test-propagation-max-denom.ratio-rational-div.1
    test-propagation-max-denom.ratio-rational-div.2
    test-propagation-max-denom.integer-ratio-plus.1
    test-propagation-max-denom.integer-ratio-plus.2
    test-propagation-max-denom.ratio-integer-plus.1
    test-propagation-max-denom.ratio-integer-plus.2
    test-propagation-max-denom.rational-ratio-plus.1
    test-propagation-max-denom.rational-ratio-plus.2
    test-propagation-max-denom.ratio-rational-plus.1
    test-propagation-max-denom.ratio-rational-plus.2
    test-propagation-max-denom.integer-ratio-times.1
    test-propagation-max-denom.integer-ratio-times.2
    test-propagation-max-denom.ratio-integer-times.1
    test-propagation-max-denom.ratio-integer-times.2
    test-propagation-max-denom.rational-ratio-times.1
    test-propagation-max-denom.rational-ratio-times.2
    test-propagation-max-denom.ratio-rational-times.1
    test-propagation-max-denom.ratio-rational-times.2
    test-propagation-max-denom.integer-ratio-minus.1
    test-propagation-max-denom.integer-ratio-minus.2
    test-propagation-max-denom.ratio-integer-minus.1
    test-propagation-max-denom.ratio-integer-minus.2
    test-propagation-max-denom.rational-ratio-minus.1
    test-propagation-max-denom.rational-ratio-minus.2
    test-propagation-max-denom.ratio-rational-minus.1
    test-propagation-max-denom.ratio-rational-minus.2
    test-maxv-max-denom
    test-minv-max-denom
    test-floatpv
    test-notv-floatpv
    test-share-equalv-rational-enum-max-denom))

(cl:defun rational-ordeal ()
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
      (dolist (test *rational-ordeal-tests*)
        (run-test test))
      (if bug?
          (progn (format t "Screamer rational/ratio tests have a bug.~%") nil)
          (progn (format t "All Rational tests passed.~%") t)))))
