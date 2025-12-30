;;Instance with 9x7x3 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y0z1 - location
x0y0z2 - location
x0y1z0 - location
x0y1z1 - location
x0y1z2 - location
x0y2z0 - location
x0y2z1 - location
x0y2z2 - location
x0y3z0 - location
x0y3z1 - location
x0y3z2 - location
x0y4z0 - location
x0y4z1 - location
x0y4z2 - location
x0y5z0 - location
x0y5z1 - location
x0y5z2 - location
x0y6z0 - location
x0y6z1 - location
x0y6z2 - location
x1y0z0 - location
x1y0z1 - location
x1y0z2 - location
x1y1z0 - location
x1y1z1 - location
x1y1z2 - location
x1y2z0 - location
x1y2z1 - location
x1y2z2 - location
x1y3z0 - location
x1y3z1 - location
x1y3z2 - location
x1y4z0 - location
x1y4z1 - location
x1y4z2 - location
x1y5z0 - location
x1y5z1 - location
x1y5z2 - location
x1y6z0 - location
x1y6z1 - location
x1y6z2 - location
x2y0z0 - location
x2y0z1 - location
x2y0z2 - location
x2y1z0 - location
x2y1z1 - location
x2y1z2 - location
x2y2z0 - location
x2y2z1 - location
x2y2z2 - location
x2y3z0 - location
x2y3z1 - location
x2y3z2 - location
x2y4z0 - location
x2y4z1 - location
x2y4z2 - location
x2y5z0 - location
x2y5z1 - location
x2y5z2 - location
x2y6z0 - location
x2y6z1 - location
x2y6z2 - location
x3y0z0 - location
x3y0z1 - location
x3y0z2 - location
x3y1z0 - location
x3y1z1 - location
x3y1z2 - location
x3y2z0 - location
x3y2z1 - location
x3y2z2 - location
x3y3z0 - location
x3y3z1 - location
x3y3z2 - location
x3y4z0 - location
x3y4z1 - location
x3y4z2 - location
x3y5z0 - location
x3y5z1 - location
x3y5z2 - location
x3y6z0 - location
x3y6z1 - location
x3y6z2 - location
x4y0z0 - location
x4y0z1 - location
x4y0z2 - location
x4y1z0 - location
x4y1z1 - location
x4y1z2 - location
x4y2z0 - location
x4y2z1 - location
x4y2z2 - location
x4y3z0 - location
x4y3z1 - location
x4y3z2 - location
x4y4z0 - location
x4y4z1 - location
x4y4z2 - location
x4y5z0 - location
x4y5z1 - location
x4y5z2 - location
x4y6z0 - location
x4y6z1 - location
x4y6z2 - location
x5y0z0 - location
x5y0z1 - location
x5y0z2 - location
x5y1z0 - location
x5y1z1 - location
x5y1z2 - location
x5y2z0 - location
x5y2z1 - location
x5y2z2 - location
x5y3z0 - location
x5y3z1 - location
x5y3z2 - location
x5y4z0 - location
x5y4z1 - location
x5y4z2 - location
x5y5z0 - location
x5y5z1 - location
x5y5z2 - location
x5y6z0 - location
x5y6z1 - location
x5y6z2 - location
x6y0z0 - location
x6y0z1 - location
x6y0z2 - location
x6y1z0 - location
x6y1z1 - location
x6y1z2 - location
x6y2z0 - location
x6y2z1 - location
x6y2z2 - location
x6y3z0 - location
x6y3z1 - location
x6y3z2 - location
x6y4z0 - location
x6y4z1 - location
x6y4z2 - location
x6y5z0 - location
x6y5z1 - location
x6y5z2 - location
x6y6z0 - location
x6y6z1 - location
x6y6z2 - location
x7y0z0 - location
x7y0z1 - location
x7y0z2 - location
x7y1z0 - location
x7y1z1 - location
x7y1z2 - location
x7y2z0 - location
x7y2z1 - location
x7y2z2 - location
x7y3z0 - location
x7y3z1 - location
x7y3z2 - location
x7y4z0 - location
x7y4z1 - location
x7y4z2 - location
x7y5z0 - location
x7y5z1 - location
x7y5z2 - location
x7y6z0 - location
x7y6z1 - location
x7y6z2 - location
x8y0z0 - location
x8y0z1 - location
x8y0z2 - location
x8y1z0 - location
x8y1z1 - location
x8y1z2 - location
x8y2z0 - location
x8y2z1 - location
x8y2z2 - location
x8y3z0 - location
x8y3z1 - location
x8y3z2 - location
x8y4z0 - location
x8y4z1 - location
x8y4z2 - location
x8y5z0 - location
x8y5z1 - location
x8y5z2 - location
x8y6z0 - location
x8y6z1 - location
x8y6z2 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 9) 
 (= (min_y) 0)  (= (max_y) 7) 
 (= (min_z) 0)  (= (max_z) 3) 
(= (xl x0y0z0) 0)
(= (yl x0y0z0) 0)
(= (zl x0y0z0) 0)
(= (xl x0y0z1) 0)
(= (yl x0y0z1) 0)
(= (zl x0y0z1) 1)
(= (xl x0y0z2) 0)
(= (yl x0y0z2) 0)
(= (zl x0y0z2) 2)
(= (xl x0y1z0) 0)
(= (yl x0y1z0) 1)
(= (zl x0y1z0) 0)
(= (xl x0y1z1) 0)
(= (yl x0y1z1) 1)
(= (zl x0y1z1) 1)
(= (xl x0y1z2) 0)
(= (yl x0y1z2) 1)
(= (zl x0y1z2) 2)
(= (xl x0y2z0) 0)
(= (yl x0y2z0) 2)
(= (zl x0y2z0) 0)
(= (xl x0y2z1) 0)
(= (yl x0y2z1) 2)
(= (zl x0y2z1) 1)
(= (xl x0y2z2) 0)
(= (yl x0y2z2) 2)
(= (zl x0y2z2) 2)
(= (xl x0y3z0) 0)
(= (yl x0y3z0) 3)
(= (zl x0y3z0) 0)
(= (xl x0y3z1) 0)
(= (yl x0y3z1) 3)
(= (zl x0y3z1) 1)
(= (xl x0y3z2) 0)
(= (yl x0y3z2) 3)
(= (zl x0y3z2) 2)
(= (xl x0y4z0) 0)
(= (yl x0y4z0) 4)
(= (zl x0y4z0) 0)
(= (xl x0y4z1) 0)
(= (yl x0y4z1) 4)
(= (zl x0y4z1) 1)
(= (xl x0y4z2) 0)
(= (yl x0y4z2) 4)
(= (zl x0y4z2) 2)
(= (xl x0y5z0) 0)
(= (yl x0y5z0) 5)
(= (zl x0y5z0) 0)
(= (xl x0y5z1) 0)
(= (yl x0y5z1) 5)
(= (zl x0y5z1) 1)
(= (xl x0y5z2) 0)
(= (yl x0y5z2) 5)
(= (zl x0y5z2) 2)
(= (xl x0y6z0) 0)
(= (yl x0y6z0) 6)
(= (zl x0y6z0) 0)
(= (xl x0y6z1) 0)
(= (yl x0y6z1) 6)
(= (zl x0y6z1) 1)
(= (xl x0y6z2) 0)
(= (yl x0y6z2) 6)
(= (zl x0y6z2) 2)
(= (xl x1y0z0) 1)
(= (yl x1y0z0) 0)
(= (zl x1y0z0) 0)
(= (xl x1y0z1) 1)
(= (yl x1y0z1) 0)
(= (zl x1y0z1) 1)
(= (xl x1y0z2) 1)
(= (yl x1y0z2) 0)
(= (zl x1y0z2) 2)
(= (xl x1y1z0) 1)
(= (yl x1y1z0) 1)
(= (zl x1y1z0) 0)
(= (xl x1y1z1) 1)
(= (yl x1y1z1) 1)
(= (zl x1y1z1) 1)
(= (xl x1y1z2) 1)
(= (yl x1y1z2) 1)
(= (zl x1y1z2) 2)
(= (xl x1y2z0) 1)
(= (yl x1y2z0) 2)
(= (zl x1y2z0) 0)
(= (xl x1y2z1) 1)
(= (yl x1y2z1) 2)
(= (zl x1y2z1) 1)
(= (xl x1y2z2) 1)
(= (yl x1y2z2) 2)
(= (zl x1y2z2) 2)
(= (xl x1y3z0) 1)
(= (yl x1y3z0) 3)
(= (zl x1y3z0) 0)
(= (xl x1y3z1) 1)
(= (yl x1y3z1) 3)
(= (zl x1y3z1) 1)
(= (xl x1y3z2) 1)
(= (yl x1y3z2) 3)
(= (zl x1y3z2) 2)
(= (xl x1y4z0) 1)
(= (yl x1y4z0) 4)
(= (zl x1y4z0) 0)
(= (xl x1y4z1) 1)
(= (yl x1y4z1) 4)
(= (zl x1y4z1) 1)
(= (xl x1y4z2) 1)
(= (yl x1y4z2) 4)
(= (zl x1y4z2) 2)
(= (xl x1y5z0) 1)
(= (yl x1y5z0) 5)
(= (zl x1y5z0) 0)
(= (xl x1y5z1) 1)
(= (yl x1y5z1) 5)
(= (zl x1y5z1) 1)
(= (xl x1y5z2) 1)
(= (yl x1y5z2) 5)
(= (zl x1y5z2) 2)
(= (xl x1y6z0) 1)
(= (yl x1y6z0) 6)
(= (zl x1y6z0) 0)
(= (xl x1y6z1) 1)
(= (yl x1y6z1) 6)
(= (zl x1y6z1) 1)
(= (xl x1y6z2) 1)
(= (yl x1y6z2) 6)
(= (zl x1y6z2) 2)
(= (xl x2y0z0) 2)
(= (yl x2y0z0) 0)
(= (zl x2y0z0) 0)
(= (xl x2y0z1) 2)
(= (yl x2y0z1) 0)
(= (zl x2y0z1) 1)
(= (xl x2y0z2) 2)
(= (yl x2y0z2) 0)
(= (zl x2y0z2) 2)
(= (xl x2y1z0) 2)
(= (yl x2y1z0) 1)
(= (zl x2y1z0) 0)
(= (xl x2y1z1) 2)
(= (yl x2y1z1) 1)
(= (zl x2y1z1) 1)
(= (xl x2y1z2) 2)
(= (yl x2y1z2) 1)
(= (zl x2y1z2) 2)
(= (xl x2y2z0) 2)
(= (yl x2y2z0) 2)
(= (zl x2y2z0) 0)
(= (xl x2y2z1) 2)
(= (yl x2y2z1) 2)
(= (zl x2y2z1) 1)
(= (xl x2y2z2) 2)
(= (yl x2y2z2) 2)
(= (zl x2y2z2) 2)
(= (xl x2y3z0) 2)
(= (yl x2y3z0) 3)
(= (zl x2y3z0) 0)
(= (xl x2y3z1) 2)
(= (yl x2y3z1) 3)
(= (zl x2y3z1) 1)
(= (xl x2y3z2) 2)
(= (yl x2y3z2) 3)
(= (zl x2y3z2) 2)
(= (xl x2y4z0) 2)
(= (yl x2y4z0) 4)
(= (zl x2y4z0) 0)
(= (xl x2y4z1) 2)
(= (yl x2y4z1) 4)
(= (zl x2y4z1) 1)
(= (xl x2y4z2) 2)
(= (yl x2y4z2) 4)
(= (zl x2y4z2) 2)
(= (xl x2y5z0) 2)
(= (yl x2y5z0) 5)
(= (zl x2y5z0) 0)
(= (xl x2y5z1) 2)
(= (yl x2y5z1) 5)
(= (zl x2y5z1) 1)
(= (xl x2y5z2) 2)
(= (yl x2y5z2) 5)
(= (zl x2y5z2) 2)
(= (xl x2y6z0) 2)
(= (yl x2y6z0) 6)
(= (zl x2y6z0) 0)
(= (xl x2y6z1) 2)
(= (yl x2y6z1) 6)
(= (zl x2y6z1) 1)
(= (xl x2y6z2) 2)
(= (yl x2y6z2) 6)
(= (zl x2y6z2) 2)
(= (xl x3y0z0) 3)
(= (yl x3y0z0) 0)
(= (zl x3y0z0) 0)
(= (xl x3y0z1) 3)
(= (yl x3y0z1) 0)
(= (zl x3y0z1) 1)
(= (xl x3y0z2) 3)
(= (yl x3y0z2) 0)
(= (zl x3y0z2) 2)
(= (xl x3y1z0) 3)
(= (yl x3y1z0) 1)
(= (zl x3y1z0) 0)
(= (xl x3y1z1) 3)
(= (yl x3y1z1) 1)
(= (zl x3y1z1) 1)
(= (xl x3y1z2) 3)
(= (yl x3y1z2) 1)
(= (zl x3y1z2) 2)
(= (xl x3y2z0) 3)
(= (yl x3y2z0) 2)
(= (zl x3y2z0) 0)
(= (xl x3y2z1) 3)
(= (yl x3y2z1) 2)
(= (zl x3y2z1) 1)
(= (xl x3y2z2) 3)
(= (yl x3y2z2) 2)
(= (zl x3y2z2) 2)
(= (xl x3y3z0) 3)
(= (yl x3y3z0) 3)
(= (zl x3y3z0) 0)
(= (xl x3y3z1) 3)
(= (yl x3y3z1) 3)
(= (zl x3y3z1) 1)
(= (xl x3y3z2) 3)
(= (yl x3y3z2) 3)
(= (zl x3y3z2) 2)
(= (xl x3y4z0) 3)
(= (yl x3y4z0) 4)
(= (zl x3y4z0) 0)
(= (xl x3y4z1) 3)
(= (yl x3y4z1) 4)
(= (zl x3y4z1) 1)
(= (xl x3y4z2) 3)
(= (yl x3y4z2) 4)
(= (zl x3y4z2) 2)
(= (xl x3y5z0) 3)
(= (yl x3y5z0) 5)
(= (zl x3y5z0) 0)
(= (xl x3y5z1) 3)
(= (yl x3y5z1) 5)
(= (zl x3y5z1) 1)
(= (xl x3y5z2) 3)
(= (yl x3y5z2) 5)
(= (zl x3y5z2) 2)
(= (xl x3y6z0) 3)
(= (yl x3y6z0) 6)
(= (zl x3y6z0) 0)
(= (xl x3y6z1) 3)
(= (yl x3y6z1) 6)
(= (zl x3y6z1) 1)
(= (xl x3y6z2) 3)
(= (yl x3y6z2) 6)
(= (zl x3y6z2) 2)
(= (xl x4y0z0) 4)
(= (yl x4y0z0) 0)
(= (zl x4y0z0) 0)
(= (xl x4y0z1) 4)
(= (yl x4y0z1) 0)
(= (zl x4y0z1) 1)
(= (xl x4y0z2) 4)
(= (yl x4y0z2) 0)
(= (zl x4y0z2) 2)
(= (xl x4y1z0) 4)
(= (yl x4y1z0) 1)
(= (zl x4y1z0) 0)
(= (xl x4y1z1) 4)
(= (yl x4y1z1) 1)
(= (zl x4y1z1) 1)
(= (xl x4y1z2) 4)
(= (yl x4y1z2) 1)
(= (zl x4y1z2) 2)
(= (xl x4y2z0) 4)
(= (yl x4y2z0) 2)
(= (zl x4y2z0) 0)
(= (xl x4y2z1) 4)
(= (yl x4y2z1) 2)
(= (zl x4y2z1) 1)
(= (xl x4y2z2) 4)
(= (yl x4y2z2) 2)
(= (zl x4y2z2) 2)
(= (xl x4y3z0) 4)
(= (yl x4y3z0) 3)
(= (zl x4y3z0) 0)
(= (xl x4y3z1) 4)
(= (yl x4y3z1) 3)
(= (zl x4y3z1) 1)
(= (xl x4y3z2) 4)
(= (yl x4y3z2) 3)
(= (zl x4y3z2) 2)
(= (xl x4y4z0) 4)
(= (yl x4y4z0) 4)
(= (zl x4y4z0) 0)
(= (xl x4y4z1) 4)
(= (yl x4y4z1) 4)
(= (zl x4y4z1) 1)
(= (xl x4y4z2) 4)
(= (yl x4y4z2) 4)
(= (zl x4y4z2) 2)
(= (xl x4y5z0) 4)
(= (yl x4y5z0) 5)
(= (zl x4y5z0) 0)
(= (xl x4y5z1) 4)
(= (yl x4y5z1) 5)
(= (zl x4y5z1) 1)
(= (xl x4y5z2) 4)
(= (yl x4y5z2) 5)
(= (zl x4y5z2) 2)
(= (xl x4y6z0) 4)
(= (yl x4y6z0) 6)
(= (zl x4y6z0) 0)
(= (xl x4y6z1) 4)
(= (yl x4y6z1) 6)
(= (zl x4y6z1) 1)
(= (xl x4y6z2) 4)
(= (yl x4y6z2) 6)
(= (zl x4y6z2) 2)
(= (xl x5y0z0) 5)
(= (yl x5y0z0) 0)
(= (zl x5y0z0) 0)
(= (xl x5y0z1) 5)
(= (yl x5y0z1) 0)
(= (zl x5y0z1) 1)
(= (xl x5y0z2) 5)
(= (yl x5y0z2) 0)
(= (zl x5y0z2) 2)
(= (xl x5y1z0) 5)
(= (yl x5y1z0) 1)
(= (zl x5y1z0) 0)
(= (xl x5y1z1) 5)
(= (yl x5y1z1) 1)
(= (zl x5y1z1) 1)
(= (xl x5y1z2) 5)
(= (yl x5y1z2) 1)
(= (zl x5y1z2) 2)
(= (xl x5y2z0) 5)
(= (yl x5y2z0) 2)
(= (zl x5y2z0) 0)
(= (xl x5y2z1) 5)
(= (yl x5y2z1) 2)
(= (zl x5y2z1) 1)
(= (xl x5y2z2) 5)
(= (yl x5y2z2) 2)
(= (zl x5y2z2) 2)
(= (xl x5y3z0) 5)
(= (yl x5y3z0) 3)
(= (zl x5y3z0) 0)
(= (xl x5y3z1) 5)
(= (yl x5y3z1) 3)
(= (zl x5y3z1) 1)
(= (xl x5y3z2) 5)
(= (yl x5y3z2) 3)
(= (zl x5y3z2) 2)
(= (xl x5y4z0) 5)
(= (yl x5y4z0) 4)
(= (zl x5y4z0) 0)
(= (xl x5y4z1) 5)
(= (yl x5y4z1) 4)
(= (zl x5y4z1) 1)
(= (xl x5y4z2) 5)
(= (yl x5y4z2) 4)
(= (zl x5y4z2) 2)
(= (xl x5y5z0) 5)
(= (yl x5y5z0) 5)
(= (zl x5y5z0) 0)
(= (xl x5y5z1) 5)
(= (yl x5y5z1) 5)
(= (zl x5y5z1) 1)
(= (xl x5y5z2) 5)
(= (yl x5y5z2) 5)
(= (zl x5y5z2) 2)
(= (xl x5y6z0) 5)
(= (yl x5y6z0) 6)
(= (zl x5y6z0) 0)
(= (xl x5y6z1) 5)
(= (yl x5y6z1) 6)
(= (zl x5y6z1) 1)
(= (xl x5y6z2) 5)
(= (yl x5y6z2) 6)
(= (zl x5y6z2) 2)
(= (xl x6y0z0) 6)
(= (yl x6y0z0) 0)
(= (zl x6y0z0) 0)
(= (xl x6y0z1) 6)
(= (yl x6y0z1) 0)
(= (zl x6y0z1) 1)
(= (xl x6y0z2) 6)
(= (yl x6y0z2) 0)
(= (zl x6y0z2) 2)
(= (xl x6y1z0) 6)
(= (yl x6y1z0) 1)
(= (zl x6y1z0) 0)
(= (xl x6y1z1) 6)
(= (yl x6y1z1) 1)
(= (zl x6y1z1) 1)
(= (xl x6y1z2) 6)
(= (yl x6y1z2) 1)
(= (zl x6y1z2) 2)
(= (xl x6y2z0) 6)
(= (yl x6y2z0) 2)
(= (zl x6y2z0) 0)
(= (xl x6y2z1) 6)
(= (yl x6y2z1) 2)
(= (zl x6y2z1) 1)
(= (xl x6y2z2) 6)
(= (yl x6y2z2) 2)
(= (zl x6y2z2) 2)
(= (xl x6y3z0) 6)
(= (yl x6y3z0) 3)
(= (zl x6y3z0) 0)
(= (xl x6y3z1) 6)
(= (yl x6y3z1) 3)
(= (zl x6y3z1) 1)
(= (xl x6y3z2) 6)
(= (yl x6y3z2) 3)
(= (zl x6y3z2) 2)
(= (xl x6y4z0) 6)
(= (yl x6y4z0) 4)
(= (zl x6y4z0) 0)
(= (xl x6y4z1) 6)
(= (yl x6y4z1) 4)
(= (zl x6y4z1) 1)
(= (xl x6y4z2) 6)
(= (yl x6y4z2) 4)
(= (zl x6y4z2) 2)
(= (xl x6y5z0) 6)
(= (yl x6y5z0) 5)
(= (zl x6y5z0) 0)
(= (xl x6y5z1) 6)
(= (yl x6y5z1) 5)
(= (zl x6y5z1) 1)
(= (xl x6y5z2) 6)
(= (yl x6y5z2) 5)
(= (zl x6y5z2) 2)
(= (xl x6y6z0) 6)
(= (yl x6y6z0) 6)
(= (zl x6y6z0) 0)
(= (xl x6y6z1) 6)
(= (yl x6y6z1) 6)
(= (zl x6y6z1) 1)
(= (xl x6y6z2) 6)
(= (yl x6y6z2) 6)
(= (zl x6y6z2) 2)
(= (xl x7y0z0) 7)
(= (yl x7y0z0) 0)
(= (zl x7y0z0) 0)
(= (xl x7y0z1) 7)
(= (yl x7y0z1) 0)
(= (zl x7y0z1) 1)
(= (xl x7y0z2) 7)
(= (yl x7y0z2) 0)
(= (zl x7y0z2) 2)
(= (xl x7y1z0) 7)
(= (yl x7y1z0) 1)
(= (zl x7y1z0) 0)
(= (xl x7y1z1) 7)
(= (yl x7y1z1) 1)
(= (zl x7y1z1) 1)
(= (xl x7y1z2) 7)
(= (yl x7y1z2) 1)
(= (zl x7y1z2) 2)
(= (xl x7y2z0) 7)
(= (yl x7y2z0) 2)
(= (zl x7y2z0) 0)
(= (xl x7y2z1) 7)
(= (yl x7y2z1) 2)
(= (zl x7y2z1) 1)
(= (xl x7y2z2) 7)
(= (yl x7y2z2) 2)
(= (zl x7y2z2) 2)
(= (xl x7y3z0) 7)
(= (yl x7y3z0) 3)
(= (zl x7y3z0) 0)
(= (xl x7y3z1) 7)
(= (yl x7y3z1) 3)
(= (zl x7y3z1) 1)
(= (xl x7y3z2) 7)
(= (yl x7y3z2) 3)
(= (zl x7y3z2) 2)
(= (xl x7y4z0) 7)
(= (yl x7y4z0) 4)
(= (zl x7y4z0) 0)
(= (xl x7y4z1) 7)
(= (yl x7y4z1) 4)
(= (zl x7y4z1) 1)
(= (xl x7y4z2) 7)
(= (yl x7y4z2) 4)
(= (zl x7y4z2) 2)
(= (xl x7y5z0) 7)
(= (yl x7y5z0) 5)
(= (zl x7y5z0) 0)
(= (xl x7y5z1) 7)
(= (yl x7y5z1) 5)
(= (zl x7y5z1) 1)
(= (xl x7y5z2) 7)
(= (yl x7y5z2) 5)
(= (zl x7y5z2) 2)
(= (xl x7y6z0) 7)
(= (yl x7y6z0) 6)
(= (zl x7y6z0) 0)
(= (xl x7y6z1) 7)
(= (yl x7y6z1) 6)
(= (zl x7y6z1) 1)
(= (xl x7y6z2) 7)
(= (yl x7y6z2) 6)
(= (zl x7y6z2) 2)
(= (xl x8y0z0) 8)
(= (yl x8y0z0) 0)
(= (zl x8y0z0) 0)
(= (xl x8y0z1) 8)
(= (yl x8y0z1) 0)
(= (zl x8y0z1) 1)
(= (xl x8y0z2) 8)
(= (yl x8y0z2) 0)
(= (zl x8y0z2) 2)
(= (xl x8y1z0) 8)
(= (yl x8y1z0) 1)
(= (zl x8y1z0) 0)
(= (xl x8y1z1) 8)
(= (yl x8y1z1) 1)
(= (zl x8y1z1) 1)
(= (xl x8y1z2) 8)
(= (yl x8y1z2) 1)
(= (zl x8y1z2) 2)
(= (xl x8y2z0) 8)
(= (yl x8y2z0) 2)
(= (zl x8y2z0) 0)
(= (xl x8y2z1) 8)
(= (yl x8y2z1) 2)
(= (zl x8y2z1) 1)
(= (xl x8y2z2) 8)
(= (yl x8y2z2) 2)
(= (zl x8y2z2) 2)
(= (xl x8y3z0) 8)
(= (yl x8y3z0) 3)
(= (zl x8y3z0) 0)
(= (xl x8y3z1) 8)
(= (yl x8y3z1) 3)
(= (zl x8y3z1) 1)
(= (xl x8y3z2) 8)
(= (yl x8y3z2) 3)
(= (zl x8y3z2) 2)
(= (xl x8y4z0) 8)
(= (yl x8y4z0) 4)
(= (zl x8y4z0) 0)
(= (xl x8y4z1) 8)
(= (yl x8y4z1) 4)
(= (zl x8y4z1) 1)
(= (xl x8y4z2) 8)
(= (yl x8y4z2) 4)
(= (zl x8y4z2) 2)
(= (xl x8y5z0) 8)
(= (yl x8y5z0) 5)
(= (zl x8y5z0) 0)
(= (xl x8y5z1) 8)
(= (yl x8y5z1) 5)
(= (zl x8y5z1) 1)
(= (xl x8y5z2) 8)
(= (yl x8y5z2) 5)
(= (zl x8y5z2) 2)
(= (xl x8y6z0) 8)
(= (yl x8y6z0) 6)
(= (zl x8y6z0) 0)
(= (xl x8y6z1) 8)
(= (yl x8y6z1) 6)
(= (zl x8y6z1) 1)
(= (xl x8y6z2) 8)
(= (yl x8y6z2) 6)
(= (zl x8y6z2) 2)
(= (battery-level) 39)
(= (battery-level-full) 39)
)
(:goal (and 
(visited x0y0z0)
(visited x0y0z1)
(visited x0y0z2)
(visited x0y1z0)
(visited x0y1z1)
(visited x0y1z2)
(visited x0y2z0)
(visited x0y2z1)
(visited x0y2z2)
(visited x0y3z0)
(visited x0y3z1)
(visited x0y3z2)
(visited x0y4z0)
(visited x0y4z1)
(visited x0y4z2)
(visited x0y5z0)
(visited x0y5z1)
(visited x0y5z2)
(visited x0y6z0)
(visited x0y6z1)
(visited x0y6z2)
(visited x1y0z0)
(visited x1y0z1)
(visited x1y0z2)
(visited x1y1z0)
(visited x1y1z1)
(visited x1y1z2)
(visited x1y2z0)
(visited x1y2z1)
(visited x1y2z2)
(visited x1y3z0)
(visited x1y3z1)
(visited x1y3z2)
(visited x1y4z0)
(visited x1y4z1)
(visited x1y4z2)
(visited x1y5z0)
(visited x1y5z1)
(visited x1y5z2)
(visited x1y6z0)
(visited x1y6z1)
(visited x1y6z2)
(visited x2y0z0)
(visited x2y0z1)
(visited x2y0z2)
(visited x2y1z0)
(visited x2y1z1)
(visited x2y1z2)
(visited x2y2z0)
(visited x2y2z1)
(visited x2y2z2)
(visited x2y3z0)
(visited x2y3z1)
(visited x2y3z2)
(visited x2y4z0)
(visited x2y4z1)
(visited x2y4z2)
(visited x2y5z0)
(visited x2y5z1)
(visited x2y5z2)
(visited x2y6z0)
(visited x2y6z1)
(visited x2y6z2)
(visited x3y0z0)
(visited x3y0z1)
(visited x3y0z2)
(visited x3y1z0)
(visited x3y1z1)
(visited x3y1z2)
(visited x3y2z0)
(visited x3y2z1)
(visited x3y2z2)
(visited x3y3z0)
(visited x3y3z1)
(visited x3y3z2)
(visited x3y4z0)
(visited x3y4z1)
(visited x3y4z2)
(visited x3y5z0)
(visited x3y5z1)
(visited x3y5z2)
(visited x3y6z0)
(visited x3y6z1)
(visited x3y6z2)
(visited x4y0z0)
(visited x4y0z1)
(visited x4y0z2)
(visited x4y1z0)
(visited x4y1z1)
(visited x4y1z2)
(visited x4y2z0)
(visited x4y2z1)
(visited x4y2z2)
(visited x4y3z0)
(visited x4y3z1)
(visited x4y3z2)
(visited x4y4z0)
(visited x4y4z1)
(visited x4y4z2)
(visited x4y5z0)
(visited x4y5z1)
(visited x4y5z2)
(visited x4y6z0)
(visited x4y6z1)
(visited x4y6z2)
(visited x5y0z0)
(visited x5y0z1)
(visited x5y0z2)
(visited x5y1z0)
(visited x5y1z1)
(visited x5y1z2)
(visited x5y2z0)
(visited x5y2z1)
(visited x5y2z2)
(visited x5y3z0)
(visited x5y3z1)
(visited x5y3z2)
(visited x5y4z0)
(visited x5y4z1)
(visited x5y4z2)
(visited x5y5z0)
(visited x5y5z1)
(visited x5y5z2)
(visited x5y6z0)
(visited x5y6z1)
(visited x5y6z2)
(visited x6y0z0)
(visited x6y0z1)
(visited x6y0z2)
(visited x6y1z0)
(visited x6y1z1)
(visited x6y1z2)
(visited x6y2z0)
(visited x6y2z1)
(visited x6y2z2)
(visited x6y3z0)
(visited x6y3z1)
(visited x6y3z2)
(visited x6y4z0)
(visited x6y4z1)
(visited x6y4z2)
(visited x6y5z0)
(visited x6y5z1)
(visited x6y5z2)
(visited x6y6z0)
(visited x6y6z1)
(visited x6y6z2)
(visited x7y0z0)
(visited x7y0z1)
(visited x7y0z2)
(visited x7y1z0)
(visited x7y1z1)
(visited x7y1z2)
(visited x7y2z0)
(visited x7y2z1)
(visited x7y2z2)
(visited x7y3z0)
(visited x7y3z1)
(visited x7y3z2)
(visited x7y4z0)
(visited x7y4z1)
(visited x7y4z2)
(visited x7y5z0)
(visited x7y5z1)
(visited x7y5z2)
(visited x7y6z0)
(visited x7y6z1)
(visited x7y6z2)
(visited x8y0z0)
(visited x8y0z1)
(visited x8y0z2)
(visited x8y1z0)
(visited x8y1z1)
(visited x8y1z2)
(visited x8y2z0)
(visited x8y2z1)
(visited x8y2z2)
(visited x8y3z0)
(visited x8y3z1)
(visited x8y3z2)
(visited x8y4z0)
(visited x8y4z1)
(visited x8y4z2)
(visited x8y5z0)
(visited x8y5z1)
(visited x8y5z2)
(visited x8y6z0)
(visited x8y6z1)
(visited x8y6z2)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
