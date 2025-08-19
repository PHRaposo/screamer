
(in-package :screamer)

; ------------------------------------------------------------------ -;
;; EXAMPLES FROM https://www.cuemath.com/algebra/objective-function/ ;;
; ------------------------------------------------------------------- ;

;; DEFINITION:

; Objective function is prominently used to represent and solve the optimization problems of
; linear programming. The objective function is of the form Z = ax + by, where x, y are the
; decision variables. The function Z = ax + by is to be maximized or minimized to find the
; optimal solution. Here the objective function is governed by the constraints x > 0, y > 0.
; The optimization problems which needs to maximize the profit, minimize the cost, or minimize
; the use of resources, makes use of an objective function.

;; Example 1: A furniture dealer has to buy chairs and tables and he has total available money of $50,000 for investment.
; The cost of a table is $2500, and the cost of a chair is $500. He has storage space for only 60 pieces, and he can make
; a profit of $300 on a table and $100 on a chair.

; Express this as an objective function and also find the constraints.

; Solution:

; Let us consider the number of tables as x and the number of chairs as y. The cost of a table is $2500, and the cost of a
; chair is $500, and the total cost cannot exceed more than $50,000.

; Constraint - I: 2500x + 500y < 50000 OR 5x + y < 100.

; The dealer does not have storage space for more than 60 pieces. This can be represented as a second constraint.

; Constraint - II: x + y < 60

; There is a profit of $300 on the table and $100 on the chair. The aim is to optimize the profits and this can be
; represented as the objective function.

; Objective Function: Z = 300x + 100y.

; Therefore, the constraints are 5x + y < 100, x + y < 60, and the objective function is Z = 300x + 100y.

(defun best-value-example.1 ()
(let ((x (an-integer-abovev 1))
      (y (an-integer-abovev 1)))	
 (assert! (<=v (+v (*v 5 x) y) 100));<== Constraint - I
 (assert! (<=v (+v x y) 60));<== Constraint - II
 (best-value
  (solution (list x y) (static-ordering #'linear-force))  
  (+v (*v 300 x) (*v 100 y)))));<== Objective function
   
; (best-value-example.1)
; > ((10 50) 8000)

(defun optimize-value-example.1 ()
(let ((x (an-integer-abovev 1))
      (y (an-integer-abovev 1)))	
 (assert! (<=v (+v (*v 5 x) y) 100));<== Constraint - I
 (assert! (<=v (+v x y) 60));<== Constraint - II
 (optimize-value 'max
  (solution (list x y) (static-ordering #'linear-force))  
  (+v (*v 300 x) (*v 100 y)))))

; (optimize-value-example.1)
; > ((10 50) 8000)

;; Example 2: A manufacturing company makes two kinds of instruments. The first instrument requires 9 hours of fabrication
; time and one hour of labor time for finishing. The second model requires 12 hours for fabricating and 2 hours of labor time
; for finishing. The total time available for fabricating and finishing is 180 hours and 30 hours respectively. The company makes
; a profit of $800 on the first instrument and $1200 on the second instrument.

; Express this linear programming problem as an objective function and also find the constraint involved.

; Solution:

; Let the two kinds of instruments be such that there are x number of the first instrument and y number of the second instrument.
; Given that 9 hour of fabrication time and 1 ;hour of finishing time is needed for each of the x number of the first instrument.
; Also 12 hours of fabricating time and 2 hours of finishing time is required for y number ;of the second instrument. Further,
; there are a total of 180 hours for fabricating and 30 hours for finishing. These can be defined as the two constraints.

; Constraint - 1 (For Finishing): 9x + 12y < 180 OR 3x + 4y < 60

; Constraint - II (For Fabricating): x + 2y < 30.

; The company makes a profit of $800 on each of the x numbers of the first instrument, and a profit of $1200 on each of the y numbers of the second instrument. The aim is to ;maximize the profits and this can be represented as an objective function.

; Objective Function: Z = 800x + 1200y

; Therefore the two constraints are 3x + 4y < 60, x + 2y < 30, and the objective function is Z = 800x + 1200y.

(defun best-value-example.2 ()
(let ((x (an-integer-abovev 1))
      (y (an-integer-abovev 1)))	
 (assert! (<=v (+v (*v 3 x) (*v 4 y)) 60))
 (assert! (<=v (+v x (*v 2 y)) 30))
 (best-value
  (solution (list x y) (static-ordering #'linear-force))
  (+v (*v 800 x) (*v 1200 y)))))
	 
; (best-value-example.2)  
; > ((1 14) 17600)

(defun optimize-value-example.2 ()
(let ((x (an-integer-abovev 1))
      (y (an-integer-abovev 1)))	
 (assert! (<=v (+v (*v 3 x) (*v 4 y)) 60))
 (assert! (<=v (+v x (*v 2 y)) 30))
 (optimize-value 'max
  (solution (list x y) (static-ordering #'linear-force))
  (+v (*v 800 x) (*v 1200 y)))))

; (optimize-value-example.1)  
;> ((1 14) 17600)

; ----------------------------------------------------------------- ;
;; EXAMPLES FROM https://www.geeksforgeeks.org/objective-function/ ;;
; ----------------------------------------------------------------- ;

; Maximization Objective Function

; In this type, we usually aim to maximize the objective function. The vertices that are found after graphing the
; constraints have a tendency to generate the maximum value of the objective function.

; Example: A man invests at most 8 hrs of time in making wallets and school bags. He invests 2 hrs in making wallets
; and 4 hr in school bags. He targets to make at most 5 wallets and school bags and wants to sell them and generate
; a profit of Rs 20 on a wallet and Rs 100 on a school bag. Find the objective function.

; Solution:

; Let x be the number of rotis and y be the number of bread.

; A man can invest a maximum of 8 hours by investing 2 hours on making a wallet and 4 hour on making a school bag.
; Therefore the first constraint equation is

; 2x + 4y <= 8

; ⇒ x + 2y <= 4

; The maximum number he can make is 5

; x+y <= 5

; Let the objective function be denoted by Z

; Therefore Z = 20x + 100y

(defun best-value-example.3 ()
(let ((x (an-integer-abovev 1))
      (y (an-integer-abovev 1)))	
 (assert! (<=v (+v x (*v 2 y)) 4))
 (assert! (<=v (+v x y) 5))
 (best-value
  (solution (list x y) (static-ordering #'linear-force))
  (+v (*v 20 x) (*v 100 y)))))
 
; (best-value-example.3)
; > ((2 1) 140)

(defun optimize-value-example.3 ()
(let ((x (an-integer-abovev 1))
      (y (an-integer-abovev 1)))	
 (assert! (<=v (+v x (*v 2 y)) 4))
 (assert! (<=v (+v x y) 5))
 (optimize-value 'max
  (solution (list x y) (static-ordering #'linear-force))
  (+v (*v 20 x) (*v 100 y)))))

;(optimize-value-example.3)
; > ((2 1) 140)

; -------------------------------- ;
; Minimization Objective Function
; -------------------------------- ;
 
; In this type, we usually aim to minimize the objective function. The vertices that are found after graphing the
; constraints have a tendency to generate the minimum value of the objective function.

; Example: Given the sum of the two variables is at least 20. It is given one variable is greater than equal to 9.
; Derive the objective function if the cost of one variable is 2 units and the cost of another variable is 9 units.

; Solution:

; Let x and y be the two variables. It is given sum of the two variables should be at least 20.

; x+y >= 20

; and x >= 9

; Above two inequalities are constraints for the following objective function.

; Let the objective function be denoted by Z. Therefore Z is

; Z = 2x + 9y

(defun optimize-value-example.4 ()
(let ((x (an-integer-betweenv 9 20))
      (y (an-integer-betweenv 1 20)))
 (assert! (>=v (+v x y) 20))
 (optimize-value 'min
  (solution (list x y)
  (static-ordering #'linear-force))  
  (+v (*v 2 x) (*v 9 y)))))
 
; (optimize-value-example.4)
; > ((19 1) 47)

(defparameter *best-value-tests*
  (list #'best-value-example.1
        #'optimize-value-example.1
        #'best-value-example.2
        #'optimize-value-example.2
        #'best-value-example.3
        #'optimize-value-example.3
        #'optimize-value-example.4))

(cl:defun test-best-optimize-value ()
  (let ((expected '(((10 50) 8000)
                    ((10 50) 8000)
                    ((1 14) 17600)
                    ((1 14) 17600)
                    ((2 1) 140)
                    ((2 1) 140)
                    ((19 1) 47)))
        (bug? nil))
    (flet ((run-test (fn expect)
            (let ((result
                    (handler-case
                        (funcall fn)
                      (error (e)
                        (format t "Error in ~A: ~A~%" fn e)
                        nil))))
              (unless (equal-set? result expect)
                (format t "~%Test failed: ~A~%" fn)
                (setf bug? t)))))
      (mapcar #'run-test *best-value-tests* expected)
      (if bug?
          (progn (format t "Screamer best-value and optimize-value tests have a bug.~%") nil)
          (progn (format t "All best-value and optimize-value tests passed.~%") t)))))
