;;;; Copyright 2010, Nikodemus Siivola <nikodemus@sb-studio.net>
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :screamer-user)

(define-screamer-package :screamer-tests
  (:use :cl :hu.dwim.stefil)
  (:export #:test-screamer #:screamer-tests))

(in-package :screamer-tests)

(use-package :alexandria)

(defun test-screamer (&optional no-debug)
  (flet ((test ()
           (eql 0 (getf (extract-test-run-statistics (screamer-tests))
                        :number-of-failures))))
    (if no-debug
        (without-debugging (test))
        (test))))

(defsuite (screamer-tests :in root-suite) ()
  (run-child-tests))

(in-suite screamer-tests)

(defun eval-when/ct ()
  (let ((x (either :a :b)))
    (declare (ignorable x))
    (or (eval-when (:compile-toplevel)
          x)
        t)))

(defun eval-when/lt ()
  (let ((x (either :a :b)))
    (declare (ignorable x))
    (or (eval-when (:load-toplevel)
          x)
        t)))

(defun eval-when/ex ()
  (let ((x (either :a :b)))
    (or (eval-when (:execute)
          x)
        t)))

(deftest eval-when.situations ()
  (is (equal '(t t) (all-values (eval-when/ct))))
  (is (equal '(t t) (all-values (eval-when/lt))))
  (is (equal '(:a :b) (all-values (eval-when/ex)))))

(defmacro evil-ding (form &environment env)
  (let ((exp (macroexpand form env)))
    `(or ,exp 'ding)))

(defun multiple-value-call-nondeterministic.ding ()
  (let ((bar (lambda (cont &rest args)
               (if args
                   (either (car args) (apply-nondeterministic cont cont (cdr args)))
                   (fail)))))
    (evil-ding
     (multiple-value-call-nondeterministic bar bar (values 1 nil 2) (values 4 nil 5)))))

(deftest multiple-value-call-nondeterministic.1 ()
  (is (equal '(1 ding 2 4 ding 5)
             (all-values (multiple-value-call-nondeterministic.ding)))))

(deftest a-member-of-vector ()
  (is (equal '() (all-values (a-member-of ""))))
  (is (equal '(#\a) (all-values (a-member-of "a"))))
  (is (equal '(#\a #\b) (all-values (a-member-of "ab"))))
  (is (equal '(#\a #\b #\c) (all-values (a-member-of "abc")))))

(deftest prime-ordeal ()
  (is (primordial::test1))
  (is (primordial::test2))
  (is (primordial::test3))
  (is (primordial::test4))
  (is (primordial::test5))
  (is (primordial::test6))
  (is (primordial::test11))
  (is (primordial::test12))
  (is (primordial::test13))
  (is (primordial::test14))
  (is (primordial::test15))
  (is (primordial::test16))
  (is (primordial::test17))
  (is (primordial::test18))
  (is (primordial::test19))
  (is (primordial::test20))
  (is (primordial::test21))
  (is (primordial::test22))
  (is (primordial::test23))
  (is (primordial::test24))
  (is (primordial::test25))
  (is (primordial::test26))
  (is (primordial::test27))
  (is (primordial::test28))
  (is (primordial::test29))
  (is (primordial::test30))
  (is (primordial::test31))
  (is (primordial::test32))
  (is (primordial::test33))
  (is (primordial::test34))
  (is (primordial::test35))
  (is (primordial::test36))
  (is (primordial::test37))
  (is (primordial::test38))
  (is (primordial::test39))
  (is (primordial::test40))
  (is (primordial::test41))
  (is (primordial::test42))
  (is (primordial::test43))
  (is (primordial::test44))
  (is (primordial::test45))
  (is (primordial::test46))
  (is (primordial::test47))
  (is (primordial::test48))
  (is (primordial::test49))
  (is (primordial::test50))
  (is (primordial::test51))
  (is (primordial::test52))
  (is (primordial::test53))
  (is (primordial::test54))
  (is (primordial::test55))
  (is (primordial::test56))
  (is (primordial::test57))
  (is (primordial::test58))
  (is (primordial::test59))
  (is (primordial::test60))
  (is (primordial::test61))
  (is (primordial::test62))
  (is (primordial::test63))
  (is (primordial::test64))
  (is (primordial::test65))
  (is (primordial::test66))
  (is (primordial::test67))
  (is (primordial::test68))
  (is (primordial::test69))
  (is (primordial::test70))
  (is (primordial::test71))
  (is (primordial::test72)))

(deftest test-trail ()
  (is (equal '(t t t)
             (all-values
               (let* ((unwind nil)
                      (x (either 1 2 3)))
                 (trail (lambda () (push x unwind)))
                 (ecase x
                   (1 (is (null unwind)))
                   (2 (is (equal '(1) unwind)))
                   (3 (is (equal '(2 1) unwind)))))))))

(deftest test-count-failures ()
  (is (equal "Failures         =          5"
             (with-output-to-string (*standard-output*)
               (is (equal '(:a 5)
                          (count-failures
                            (one-value
                                (let ((x (either 1 2 3 4 5 :a)))
                                  (unless (keywordp x)
                                    (fail))
                                  ;; FIXME: leak, but keeping it for backwards compatibility
                                  (list x screamer::failure-count))))))))))

(deftest count-truesv.1 ()
  (is (eq nil
          (let* ((x (a-booleanv))
                 (y (a-booleanv))
                 (z (a-booleanv))
                 (n (count-truesv x y z)))
            (assert! x)
            (assert! y)
            (assert! (=v n 2))
            (value-of z)))))

(deftest count-truesv.2 ()
  (is (= 2
         (let* ((x (a-booleanv))
                (y (a-booleanv))
                (z (a-booleanv))
                (n (count-truesv x y z)))
           (assert! x)
           (assert! y)
           (assert! (notv z))
           (value-of n)))))

(deftest test-minv.1 ()
  (is (= 42
         (let ((x (a-member-ofv '(:a 42))))
           (minv x)))))

(deftest test-maxv.1 ()
  (is (= 42
         (let ((x (a-member-ofv '(:a 42))))
           (maxv x)))))

(deftest share!-bugs ()
  (flet ((foo (list1 list2)
           (let ((v1 (a-member-ofv list1))
                 (v2 (a-member-ofv list2)))
             (assert! (equalv v1 v2))
             (value-of v1))))
    (is (eq :a (foo '(:a :b) '(:c :d :a))))
    (is (eq t (foo '(t nil) '(t :a))))
    (is (eql 3 (foo '(1 2 3) '(nil t 3))))
    (is (eql 3 (foo '(1 2 3) '(nil t 3 4))))
    (is (eql 3 (foo '(nil t 3 4 -1) '(1 2 3))))
    (let ((xs (all-values
                (linear-force (foo '(nil t 3 4 -1) '(1 2 3 t))))))
      (is (or (equal '(3 t) xs)
              (equal '(t 3) xs))))))

(deftest test-upper-bounding-failures ()
  (let ((screamer::*screamer-max-failures* 20))
    (is (not (possibly? (> (an-integer-below 20) 20))))
    (is (not (possibly? (solution (>v (an-integer-belowv 20) 20)
                                  (static-ordering #'linear-force)))))))

(deftest test-division-does-not-force-quotient-to-integer ()
    (is (equal
         (let* ((x (an-integer-betweenv 1 10))
                (y (an-integer-betweenv 1 10))
                (z (/v x y)))
           (all-values (solution (list x y z) (static-ordering #'linear-force))))
         `((1 1 1) (1 2 1/2) (1 3 1/3) (1 4 1/4) (1 5 1/5) (1 6 1/6) (1 7 1/7) (1 8 1/8)
           (1 9 1/9) (1 10 1/10) (2 1 2) (2 2 1) (2 3 2/3) (2 4 1/2) (2 5 2/5) (2 6 1/3)
           (2 7 2/7) (2 8 1/4) (2 9 2/9) (2 10 1/5) (3 1 3) (3 2 3/2) (3 3 1) (3 4 3/4)
           (3 5 3/5) (3 6 1/2) (3 7 3/7) (3 8 3/8) (3 9 1/3) (3 10 3/10) (4 1 4) (4 2 2)
           (4 3 4/3) (4 4 1) (4 5 4/5) (4 6 2/3) (4 7 4/7) (4 8 1/2) (4 9 4/9) (4 10 2/5)
           (5 1 5) (5 2 5/2) (5 3 5/3) (5 4 5/4) (5 5 1) (5 6 5/6) (5 7 5/7) (5 8 5/8)
           (5 9 5/9) (5 10 1/2) (6 1 6) (6 2 3) (6 3 2) (6 4 3/2) (6 5 6/5) (6 6 1)
           (6 7 6/7) (6 8 3/4) (6 9 2/3) (6 10 3/5) (7 1 7) (7 2 7/2) (7 3 7/3) (7 4 7/4)
           (7 5 7/5) (7 6 7/6) (7 7 1) (7 8 7/8) (7 9 7/9) (7 10 7/10) (8 1 8) (8 2 4)
           (8 3 8/3) (8 4 2) (8 5 8/5) (8 6 4/3) (8 7 8/7) (8 8 1) (8 9 8/9) (8 10 4/5)
           (9 1 9) (9 2 9/2) (9 3 3) (9 4 9/4) (9 5 9/5) (9 6 3/2) (9 7 9/7) (9 8 9/8)
           (9 9 1) (9 10 9/10) (10 1 10) (10 2 5) (10 3 10/3) (10 4 5/2) (10 5 2)
           (10 6 5/3) (10 7 10/7) (10 8 5/4) (10 9 10/9) (10 10 1)))))
