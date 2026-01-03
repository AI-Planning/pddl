;;Instance with 4x9x5 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y0z1 - location
x0y0z2 - location
x0y0z3 - location
x0y0z4 - location
x0y1z0 - location
x0y1z1 - location
x0y1z2 - location
x0y1z3 - location
x0y1z4 - location
x0y2z0 - location
x0y2z1 - location
x0y2z2 - location
x0y2z3 - location
x0y2z4 - location
x0y3z0 - location
x0y3z1 - location
x0y3z2 - location
x0y3z3 - location
x0y3z4 - location
x0y4z0 - location
x0y4z1 - location
x0y4z2 - location
x0y4z3 - location
x0y4z4 - location
x0y5z0 - location
x0y5z1 - location
x0y5z2 - location
x0y5z3 - location
x0y5z4 - location
x0y6z0 - location
x0y6z1 - location
x0y6z2 - location
x0y6z3 - location
x0y6z4 - location
x0y7z0 - location
x0y7z1 - location
x0y7z2 - location
x0y7z3 - location
x0y7z4 - location
x0y8z0 - location
x0y8z1 - location
x0y8z2 - location
x0y8z3 - location
x0y8z4 - location
x1y0z0 - location
x1y0z1 - location
x1y0z2 - location
x1y0z3 - location
x1y0z4 - location
x1y1z0 - location
x1y1z1 - location
x1y1z2 - location
x1y1z3 - location
x1y1z4 - location
x1y2z0 - location
x1y2z1 - location
x1y2z2 - location
x1y2z3 - location
x1y2z4 - location
x1y3z0 - location
x1y3z1 - location
x1y3z2 - location
x1y3z3 - location
x1y3z4 - location
x1y4z0 - location
x1y4z1 - location
x1y4z2 - location
x1y4z3 - location
x1y4z4 - location
x1y5z0 - location
x1y5z1 - location
x1y5z2 - location
x1y5z3 - location
x1y5z4 - location
x1y6z0 - location
x1y6z1 - location
x1y6z2 - location
x1y6z3 - location
x1y6z4 - location
x1y7z0 - location
x1y7z1 - location
x1y7z2 - location
x1y7z3 - location
x1y7z4 - location
x1y8z0 - location
x1y8z1 - location
x1y8z2 - location
x1y8z3 - location
x1y8z4 - location
x2y0z0 - location
x2y0z1 - location
x2y0z2 - location
x2y0z3 - location
x2y0z4 - location
x2y1z0 - location
x2y1z1 - location
x2y1z2 - location
x2y1z3 - location
x2y1z4 - location
x2y2z0 - location
x2y2z1 - location
x2y2z2 - location
x2y2z3 - location
x2y2z4 - location
x2y3z0 - location
x2y3z1 - location
x2y3z2 - location
x2y3z3 - location
x2y3z4 - location
x2y4z0 - location
x2y4z1 - location
x2y4z2 - location
x2y4z3 - location
x2y4z4 - location
x2y5z0 - location
x2y5z1 - location
x2y5z2 - location
x2y5z3 - location
x2y5z4 - location
x2y6z0 - location
x2y6z1 - location
x2y6z2 - location
x2y6z3 - location
x2y6z4 - location
x2y7z0 - location
x2y7z1 - location
x2y7z2 - location
x2y7z3 - location
x2y7z4 - location
x2y8z0 - location
x2y8z1 - location
x2y8z2 - location
x2y8z3 - location
x2y8z4 - location
x3y0z0 - location
x3y0z1 - location
x3y0z2 - location
x3y0z3 - location
x3y0z4 - location
x3y1z0 - location
x3y1z1 - location
x3y1z2 - location
x3y1z3 - location
x3y1z4 - location
x3y2z0 - location
x3y2z1 - location
x3y2z2 - location
x3y2z3 - location
x3y2z4 - location
x3y3z0 - location
x3y3z1 - location
x3y3z2 - location
x3y3z3 - location
x3y3z4 - location
x3y4z0 - location
x3y4z1 - location
x3y4z2 - location
x3y4z3 - location
x3y4z4 - location
x3y5z0 - location
x3y5z1 - location
x3y5z2 - location
x3y5z3 - location
x3y5z4 - location
x3y6z0 - location
x3y6z1 - location
x3y6z2 - location
x3y6z3 - location
x3y6z4 - location
x3y7z0 - location
x3y7z1 - location
x3y7z2 - location
x3y7z3 - location
x3y7z4 - location
x3y8z0 - location
x3y8z1 - location
x3y8z2 - location
x3y8z3 - location
x3y8z4 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 4) 
 (= (min_y) 0)  (= (max_y) 9) 
 (= (min_z) 0)  (= (max_z) 5) 
(= (xl x0y0z0) 0)
(= (yl x0y0z0) 0)
(= (zl x0y0z0) 0)
(= (xl x0y0z1) 0)
(= (yl x0y0z1) 0)
(= (zl x0y0z1) 1)
(= (xl x0y0z2) 0)
(= (yl x0y0z2) 0)
(= (zl x0y0z2) 2)
(= (xl x0y0z3) 0)
(= (yl x0y0z3) 0)
(= (zl x0y0z3) 3)
(= (xl x0y0z4) 0)
(= (yl x0y0z4) 0)
(= (zl x0y0z4) 4)
(= (xl x0y1z0) 0)
(= (yl x0y1z0) 1)
(= (zl x0y1z0) 0)
(= (xl x0y1z1) 0)
(= (yl x0y1z1) 1)
(= (zl x0y1z1) 1)
(= (xl x0y1z2) 0)
(= (yl x0y1z2) 1)
(= (zl x0y1z2) 2)
(= (xl x0y1z3) 0)
(= (yl x0y1z3) 1)
(= (zl x0y1z3) 3)
(= (xl x0y1z4) 0)
(= (yl x0y1z4) 1)
(= (zl x0y1z4) 4)
(= (xl x0y2z0) 0)
(= (yl x0y2z0) 2)
(= (zl x0y2z0) 0)
(= (xl x0y2z1) 0)
(= (yl x0y2z1) 2)
(= (zl x0y2z1) 1)
(= (xl x0y2z2) 0)
(= (yl x0y2z2) 2)
(= (zl x0y2z2) 2)
(= (xl x0y2z3) 0)
(= (yl x0y2z3) 2)
(= (zl x0y2z3) 3)
(= (xl x0y2z4) 0)
(= (yl x0y2z4) 2)
(= (zl x0y2z4) 4)
(= (xl x0y3z0) 0)
(= (yl x0y3z0) 3)
(= (zl x0y3z0) 0)
(= (xl x0y3z1) 0)
(= (yl x0y3z1) 3)
(= (zl x0y3z1) 1)
(= (xl x0y3z2) 0)
(= (yl x0y3z2) 3)
(= (zl x0y3z2) 2)
(= (xl x0y3z3) 0)
(= (yl x0y3z3) 3)
(= (zl x0y3z3) 3)
(= (xl x0y3z4) 0)
(= (yl x0y3z4) 3)
(= (zl x0y3z4) 4)
(= (xl x0y4z0) 0)
(= (yl x0y4z0) 4)
(= (zl x0y4z0) 0)
(= (xl x0y4z1) 0)
(= (yl x0y4z1) 4)
(= (zl x0y4z1) 1)
(= (xl x0y4z2) 0)
(= (yl x0y4z2) 4)
(= (zl x0y4z2) 2)
(= (xl x0y4z3) 0)
(= (yl x0y4z3) 4)
(= (zl x0y4z3) 3)
(= (xl x0y4z4) 0)
(= (yl x0y4z4) 4)
(= (zl x0y4z4) 4)
(= (xl x0y5z0) 0)
(= (yl x0y5z0) 5)
(= (zl x0y5z0) 0)
(= (xl x0y5z1) 0)
(= (yl x0y5z1) 5)
(= (zl x0y5z1) 1)
(= (xl x0y5z2) 0)
(= (yl x0y5z2) 5)
(= (zl x0y5z2) 2)
(= (xl x0y5z3) 0)
(= (yl x0y5z3) 5)
(= (zl x0y5z3) 3)
(= (xl x0y5z4) 0)
(= (yl x0y5z4) 5)
(= (zl x0y5z4) 4)
(= (xl x0y6z0) 0)
(= (yl x0y6z0) 6)
(= (zl x0y6z0) 0)
(= (xl x0y6z1) 0)
(= (yl x0y6z1) 6)
(= (zl x0y6z1) 1)
(= (xl x0y6z2) 0)
(= (yl x0y6z2) 6)
(= (zl x0y6z2) 2)
(= (xl x0y6z3) 0)
(= (yl x0y6z3) 6)
(= (zl x0y6z3) 3)
(= (xl x0y6z4) 0)
(= (yl x0y6z4) 6)
(= (zl x0y6z4) 4)
(= (xl x0y7z0) 0)
(= (yl x0y7z0) 7)
(= (zl x0y7z0) 0)
(= (xl x0y7z1) 0)
(= (yl x0y7z1) 7)
(= (zl x0y7z1) 1)
(= (xl x0y7z2) 0)
(= (yl x0y7z2) 7)
(= (zl x0y7z2) 2)
(= (xl x0y7z3) 0)
(= (yl x0y7z3) 7)
(= (zl x0y7z3) 3)
(= (xl x0y7z4) 0)
(= (yl x0y7z4) 7)
(= (zl x0y7z4) 4)
(= (xl x0y8z0) 0)
(= (yl x0y8z0) 8)
(= (zl x0y8z0) 0)
(= (xl x0y8z1) 0)
(= (yl x0y8z1) 8)
(= (zl x0y8z1) 1)
(= (xl x0y8z2) 0)
(= (yl x0y8z2) 8)
(= (zl x0y8z2) 2)
(= (xl x0y8z3) 0)
(= (yl x0y8z3) 8)
(= (zl x0y8z3) 3)
(= (xl x0y8z4) 0)
(= (yl x0y8z4) 8)
(= (zl x0y8z4) 4)
(= (xl x1y0z0) 1)
(= (yl x1y0z0) 0)
(= (zl x1y0z0) 0)
(= (xl x1y0z1) 1)
(= (yl x1y0z1) 0)
(= (zl x1y0z1) 1)
(= (xl x1y0z2) 1)
(= (yl x1y0z2) 0)
(= (zl x1y0z2) 2)
(= (xl x1y0z3) 1)
(= (yl x1y0z3) 0)
(= (zl x1y0z3) 3)
(= (xl x1y0z4) 1)
(= (yl x1y0z4) 0)
(= (zl x1y0z4) 4)
(= (xl x1y1z0) 1)
(= (yl x1y1z0) 1)
(= (zl x1y1z0) 0)
(= (xl x1y1z1) 1)
(= (yl x1y1z1) 1)
(= (zl x1y1z1) 1)
(= (xl x1y1z2) 1)
(= (yl x1y1z2) 1)
(= (zl x1y1z2) 2)
(= (xl x1y1z3) 1)
(= (yl x1y1z3) 1)
(= (zl x1y1z3) 3)
(= (xl x1y1z4) 1)
(= (yl x1y1z4) 1)
(= (zl x1y1z4) 4)
(= (xl x1y2z0) 1)
(= (yl x1y2z0) 2)
(= (zl x1y2z0) 0)
(= (xl x1y2z1) 1)
(= (yl x1y2z1) 2)
(= (zl x1y2z1) 1)
(= (xl x1y2z2) 1)
(= (yl x1y2z2) 2)
(= (zl x1y2z2) 2)
(= (xl x1y2z3) 1)
(= (yl x1y2z3) 2)
(= (zl x1y2z3) 3)
(= (xl x1y2z4) 1)
(= (yl x1y2z4) 2)
(= (zl x1y2z4) 4)
(= (xl x1y3z0) 1)
(= (yl x1y3z0) 3)
(= (zl x1y3z0) 0)
(= (xl x1y3z1) 1)
(= (yl x1y3z1) 3)
(= (zl x1y3z1) 1)
(= (xl x1y3z2) 1)
(= (yl x1y3z2) 3)
(= (zl x1y3z2) 2)
(= (xl x1y3z3) 1)
(= (yl x1y3z3) 3)
(= (zl x1y3z3) 3)
(= (xl x1y3z4) 1)
(= (yl x1y3z4) 3)
(= (zl x1y3z4) 4)
(= (xl x1y4z0) 1)
(= (yl x1y4z0) 4)
(= (zl x1y4z0) 0)
(= (xl x1y4z1) 1)
(= (yl x1y4z1) 4)
(= (zl x1y4z1) 1)
(= (xl x1y4z2) 1)
(= (yl x1y4z2) 4)
(= (zl x1y4z2) 2)
(= (xl x1y4z3) 1)
(= (yl x1y4z3) 4)
(= (zl x1y4z3) 3)
(= (xl x1y4z4) 1)
(= (yl x1y4z4) 4)
(= (zl x1y4z4) 4)
(= (xl x1y5z0) 1)
(= (yl x1y5z0) 5)
(= (zl x1y5z0) 0)
(= (xl x1y5z1) 1)
(= (yl x1y5z1) 5)
(= (zl x1y5z1) 1)
(= (xl x1y5z2) 1)
(= (yl x1y5z2) 5)
(= (zl x1y5z2) 2)
(= (xl x1y5z3) 1)
(= (yl x1y5z3) 5)
(= (zl x1y5z3) 3)
(= (xl x1y5z4) 1)
(= (yl x1y5z4) 5)
(= (zl x1y5z4) 4)
(= (xl x1y6z0) 1)
(= (yl x1y6z0) 6)
(= (zl x1y6z0) 0)
(= (xl x1y6z1) 1)
(= (yl x1y6z1) 6)
(= (zl x1y6z1) 1)
(= (xl x1y6z2) 1)
(= (yl x1y6z2) 6)
(= (zl x1y6z2) 2)
(= (xl x1y6z3) 1)
(= (yl x1y6z3) 6)
(= (zl x1y6z3) 3)
(= (xl x1y6z4) 1)
(= (yl x1y6z4) 6)
(= (zl x1y6z4) 4)
(= (xl x1y7z0) 1)
(= (yl x1y7z0) 7)
(= (zl x1y7z0) 0)
(= (xl x1y7z1) 1)
(= (yl x1y7z1) 7)
(= (zl x1y7z1) 1)
(= (xl x1y7z2) 1)
(= (yl x1y7z2) 7)
(= (zl x1y7z2) 2)
(= (xl x1y7z3) 1)
(= (yl x1y7z3) 7)
(= (zl x1y7z3) 3)
(= (xl x1y7z4) 1)
(= (yl x1y7z4) 7)
(= (zl x1y7z4) 4)
(= (xl x1y8z0) 1)
(= (yl x1y8z0) 8)
(= (zl x1y8z0) 0)
(= (xl x1y8z1) 1)
(= (yl x1y8z1) 8)
(= (zl x1y8z1) 1)
(= (xl x1y8z2) 1)
(= (yl x1y8z2) 8)
(= (zl x1y8z2) 2)
(= (xl x1y8z3) 1)
(= (yl x1y8z3) 8)
(= (zl x1y8z3) 3)
(= (xl x1y8z4) 1)
(= (yl x1y8z4) 8)
(= (zl x1y8z4) 4)
(= (xl x2y0z0) 2)
(= (yl x2y0z0) 0)
(= (zl x2y0z0) 0)
(= (xl x2y0z1) 2)
(= (yl x2y0z1) 0)
(= (zl x2y0z1) 1)
(= (xl x2y0z2) 2)
(= (yl x2y0z2) 0)
(= (zl x2y0z2) 2)
(= (xl x2y0z3) 2)
(= (yl x2y0z3) 0)
(= (zl x2y0z3) 3)
(= (xl x2y0z4) 2)
(= (yl x2y0z4) 0)
(= (zl x2y0z4) 4)
(= (xl x2y1z0) 2)
(= (yl x2y1z0) 1)
(= (zl x2y1z0) 0)
(= (xl x2y1z1) 2)
(= (yl x2y1z1) 1)
(= (zl x2y1z1) 1)
(= (xl x2y1z2) 2)
(= (yl x2y1z2) 1)
(= (zl x2y1z2) 2)
(= (xl x2y1z3) 2)
(= (yl x2y1z3) 1)
(= (zl x2y1z3) 3)
(= (xl x2y1z4) 2)
(= (yl x2y1z4) 1)
(= (zl x2y1z4) 4)
(= (xl x2y2z0) 2)
(= (yl x2y2z0) 2)
(= (zl x2y2z0) 0)
(= (xl x2y2z1) 2)
(= (yl x2y2z1) 2)
(= (zl x2y2z1) 1)
(= (xl x2y2z2) 2)
(= (yl x2y2z2) 2)
(= (zl x2y2z2) 2)
(= (xl x2y2z3) 2)
(= (yl x2y2z3) 2)
(= (zl x2y2z3) 3)
(= (xl x2y2z4) 2)
(= (yl x2y2z4) 2)
(= (zl x2y2z4) 4)
(= (xl x2y3z0) 2)
(= (yl x2y3z0) 3)
(= (zl x2y3z0) 0)
(= (xl x2y3z1) 2)
(= (yl x2y3z1) 3)
(= (zl x2y3z1) 1)
(= (xl x2y3z2) 2)
(= (yl x2y3z2) 3)
(= (zl x2y3z2) 2)
(= (xl x2y3z3) 2)
(= (yl x2y3z3) 3)
(= (zl x2y3z3) 3)
(= (xl x2y3z4) 2)
(= (yl x2y3z4) 3)
(= (zl x2y3z4) 4)
(= (xl x2y4z0) 2)
(= (yl x2y4z0) 4)
(= (zl x2y4z0) 0)
(= (xl x2y4z1) 2)
(= (yl x2y4z1) 4)
(= (zl x2y4z1) 1)
(= (xl x2y4z2) 2)
(= (yl x2y4z2) 4)
(= (zl x2y4z2) 2)
(= (xl x2y4z3) 2)
(= (yl x2y4z3) 4)
(= (zl x2y4z3) 3)
(= (xl x2y4z4) 2)
(= (yl x2y4z4) 4)
(= (zl x2y4z4) 4)
(= (xl x2y5z0) 2)
(= (yl x2y5z0) 5)
(= (zl x2y5z0) 0)
(= (xl x2y5z1) 2)
(= (yl x2y5z1) 5)
(= (zl x2y5z1) 1)
(= (xl x2y5z2) 2)
(= (yl x2y5z2) 5)
(= (zl x2y5z2) 2)
(= (xl x2y5z3) 2)
(= (yl x2y5z3) 5)
(= (zl x2y5z3) 3)
(= (xl x2y5z4) 2)
(= (yl x2y5z4) 5)
(= (zl x2y5z4) 4)
(= (xl x2y6z0) 2)
(= (yl x2y6z0) 6)
(= (zl x2y6z0) 0)
(= (xl x2y6z1) 2)
(= (yl x2y6z1) 6)
(= (zl x2y6z1) 1)
(= (xl x2y6z2) 2)
(= (yl x2y6z2) 6)
(= (zl x2y6z2) 2)
(= (xl x2y6z3) 2)
(= (yl x2y6z3) 6)
(= (zl x2y6z3) 3)
(= (xl x2y6z4) 2)
(= (yl x2y6z4) 6)
(= (zl x2y6z4) 4)
(= (xl x2y7z0) 2)
(= (yl x2y7z0) 7)
(= (zl x2y7z0) 0)
(= (xl x2y7z1) 2)
(= (yl x2y7z1) 7)
(= (zl x2y7z1) 1)
(= (xl x2y7z2) 2)
(= (yl x2y7z2) 7)
(= (zl x2y7z2) 2)
(= (xl x2y7z3) 2)
(= (yl x2y7z3) 7)
(= (zl x2y7z3) 3)
(= (xl x2y7z4) 2)
(= (yl x2y7z4) 7)
(= (zl x2y7z4) 4)
(= (xl x2y8z0) 2)
(= (yl x2y8z0) 8)
(= (zl x2y8z0) 0)
(= (xl x2y8z1) 2)
(= (yl x2y8z1) 8)
(= (zl x2y8z1) 1)
(= (xl x2y8z2) 2)
(= (yl x2y8z2) 8)
(= (zl x2y8z2) 2)
(= (xl x2y8z3) 2)
(= (yl x2y8z3) 8)
(= (zl x2y8z3) 3)
(= (xl x2y8z4) 2)
(= (yl x2y8z4) 8)
(= (zl x2y8z4) 4)
(= (xl x3y0z0) 3)
(= (yl x3y0z0) 0)
(= (zl x3y0z0) 0)
(= (xl x3y0z1) 3)
(= (yl x3y0z1) 0)
(= (zl x3y0z1) 1)
(= (xl x3y0z2) 3)
(= (yl x3y0z2) 0)
(= (zl x3y0z2) 2)
(= (xl x3y0z3) 3)
(= (yl x3y0z3) 0)
(= (zl x3y0z3) 3)
(= (xl x3y0z4) 3)
(= (yl x3y0z4) 0)
(= (zl x3y0z4) 4)
(= (xl x3y1z0) 3)
(= (yl x3y1z0) 1)
(= (zl x3y1z0) 0)
(= (xl x3y1z1) 3)
(= (yl x3y1z1) 1)
(= (zl x3y1z1) 1)
(= (xl x3y1z2) 3)
(= (yl x3y1z2) 1)
(= (zl x3y1z2) 2)
(= (xl x3y1z3) 3)
(= (yl x3y1z3) 1)
(= (zl x3y1z3) 3)
(= (xl x3y1z4) 3)
(= (yl x3y1z4) 1)
(= (zl x3y1z4) 4)
(= (xl x3y2z0) 3)
(= (yl x3y2z0) 2)
(= (zl x3y2z0) 0)
(= (xl x3y2z1) 3)
(= (yl x3y2z1) 2)
(= (zl x3y2z1) 1)
(= (xl x3y2z2) 3)
(= (yl x3y2z2) 2)
(= (zl x3y2z2) 2)
(= (xl x3y2z3) 3)
(= (yl x3y2z3) 2)
(= (zl x3y2z3) 3)
(= (xl x3y2z4) 3)
(= (yl x3y2z4) 2)
(= (zl x3y2z4) 4)
(= (xl x3y3z0) 3)
(= (yl x3y3z0) 3)
(= (zl x3y3z0) 0)
(= (xl x3y3z1) 3)
(= (yl x3y3z1) 3)
(= (zl x3y3z1) 1)
(= (xl x3y3z2) 3)
(= (yl x3y3z2) 3)
(= (zl x3y3z2) 2)
(= (xl x3y3z3) 3)
(= (yl x3y3z3) 3)
(= (zl x3y3z3) 3)
(= (xl x3y3z4) 3)
(= (yl x3y3z4) 3)
(= (zl x3y3z4) 4)
(= (xl x3y4z0) 3)
(= (yl x3y4z0) 4)
(= (zl x3y4z0) 0)
(= (xl x3y4z1) 3)
(= (yl x3y4z1) 4)
(= (zl x3y4z1) 1)
(= (xl x3y4z2) 3)
(= (yl x3y4z2) 4)
(= (zl x3y4z2) 2)
(= (xl x3y4z3) 3)
(= (yl x3y4z3) 4)
(= (zl x3y4z3) 3)
(= (xl x3y4z4) 3)
(= (yl x3y4z4) 4)
(= (zl x3y4z4) 4)
(= (xl x3y5z0) 3)
(= (yl x3y5z0) 5)
(= (zl x3y5z0) 0)
(= (xl x3y5z1) 3)
(= (yl x3y5z1) 5)
(= (zl x3y5z1) 1)
(= (xl x3y5z2) 3)
(= (yl x3y5z2) 5)
(= (zl x3y5z2) 2)
(= (xl x3y5z3) 3)
(= (yl x3y5z3) 5)
(= (zl x3y5z3) 3)
(= (xl x3y5z4) 3)
(= (yl x3y5z4) 5)
(= (zl x3y5z4) 4)
(= (xl x3y6z0) 3)
(= (yl x3y6z0) 6)
(= (zl x3y6z0) 0)
(= (xl x3y6z1) 3)
(= (yl x3y6z1) 6)
(= (zl x3y6z1) 1)
(= (xl x3y6z2) 3)
(= (yl x3y6z2) 6)
(= (zl x3y6z2) 2)
(= (xl x3y6z3) 3)
(= (yl x3y6z3) 6)
(= (zl x3y6z3) 3)
(= (xl x3y6z4) 3)
(= (yl x3y6z4) 6)
(= (zl x3y6z4) 4)
(= (xl x3y7z0) 3)
(= (yl x3y7z0) 7)
(= (zl x3y7z0) 0)
(= (xl x3y7z1) 3)
(= (yl x3y7z1) 7)
(= (zl x3y7z1) 1)
(= (xl x3y7z2) 3)
(= (yl x3y7z2) 7)
(= (zl x3y7z2) 2)
(= (xl x3y7z3) 3)
(= (yl x3y7z3) 7)
(= (zl x3y7z3) 3)
(= (xl x3y7z4) 3)
(= (yl x3y7z4) 7)
(= (zl x3y7z4) 4)
(= (xl x3y8z0) 3)
(= (yl x3y8z0) 8)
(= (zl x3y8z0) 0)
(= (xl x3y8z1) 3)
(= (yl x3y8z1) 8)
(= (zl x3y8z1) 1)
(= (xl x3y8z2) 3)
(= (yl x3y8z2) 8)
(= (zl x3y8z2) 2)
(= (xl x3y8z3) 3)
(= (yl x3y8z3) 8)
(= (zl x3y8z3) 3)
(= (xl x3y8z4) 3)
(= (yl x3y8z4) 8)
(= (zl x3y8z4) 4)
(= (battery-level) 37)
(= (battery-level-full) 37)
)
(:goal (and 
(visited x0y0z0)
(visited x0y0z1)
(visited x0y0z2)
(visited x0y0z3)
(visited x0y0z4)
(visited x0y1z0)
(visited x0y1z1)
(visited x0y1z2)
(visited x0y1z3)
(visited x0y1z4)
(visited x0y2z0)
(visited x0y2z1)
(visited x0y2z2)
(visited x0y2z3)
(visited x0y2z4)
(visited x0y3z0)
(visited x0y3z1)
(visited x0y3z2)
(visited x0y3z3)
(visited x0y3z4)
(visited x0y4z0)
(visited x0y4z1)
(visited x0y4z2)
(visited x0y4z3)
(visited x0y4z4)
(visited x0y5z0)
(visited x0y5z1)
(visited x0y5z2)
(visited x0y5z3)
(visited x0y5z4)
(visited x0y6z0)
(visited x0y6z1)
(visited x0y6z2)
(visited x0y6z3)
(visited x0y6z4)
(visited x0y7z0)
(visited x0y7z1)
(visited x0y7z2)
(visited x0y7z3)
(visited x0y7z4)
(visited x0y8z0)
(visited x0y8z1)
(visited x0y8z2)
(visited x0y8z3)
(visited x0y8z4)
(visited x1y0z0)
(visited x1y0z1)
(visited x1y0z2)
(visited x1y0z3)
(visited x1y0z4)
(visited x1y1z0)
(visited x1y1z1)
(visited x1y1z2)
(visited x1y1z3)
(visited x1y1z4)
(visited x1y2z0)
(visited x1y2z1)
(visited x1y2z2)
(visited x1y2z3)
(visited x1y2z4)
(visited x1y3z0)
(visited x1y3z1)
(visited x1y3z2)
(visited x1y3z3)
(visited x1y3z4)
(visited x1y4z0)
(visited x1y4z1)
(visited x1y4z2)
(visited x1y4z3)
(visited x1y4z4)
(visited x1y5z0)
(visited x1y5z1)
(visited x1y5z2)
(visited x1y5z3)
(visited x1y5z4)
(visited x1y6z0)
(visited x1y6z1)
(visited x1y6z2)
(visited x1y6z3)
(visited x1y6z4)
(visited x1y7z0)
(visited x1y7z1)
(visited x1y7z2)
(visited x1y7z3)
(visited x1y7z4)
(visited x1y8z0)
(visited x1y8z1)
(visited x1y8z2)
(visited x1y8z3)
(visited x1y8z4)
(visited x2y0z0)
(visited x2y0z1)
(visited x2y0z2)
(visited x2y0z3)
(visited x2y0z4)
(visited x2y1z0)
(visited x2y1z1)
(visited x2y1z2)
(visited x2y1z3)
(visited x2y1z4)
(visited x2y2z0)
(visited x2y2z1)
(visited x2y2z2)
(visited x2y2z3)
(visited x2y2z4)
(visited x2y3z0)
(visited x2y3z1)
(visited x2y3z2)
(visited x2y3z3)
(visited x2y3z4)
(visited x2y4z0)
(visited x2y4z1)
(visited x2y4z2)
(visited x2y4z3)
(visited x2y4z4)
(visited x2y5z0)
(visited x2y5z1)
(visited x2y5z2)
(visited x2y5z3)
(visited x2y5z4)
(visited x2y6z0)
(visited x2y6z1)
(visited x2y6z2)
(visited x2y6z3)
(visited x2y6z4)
(visited x2y7z0)
(visited x2y7z1)
(visited x2y7z2)
(visited x2y7z3)
(visited x2y7z4)
(visited x2y8z0)
(visited x2y8z1)
(visited x2y8z2)
(visited x2y8z3)
(visited x2y8z4)
(visited x3y0z0)
(visited x3y0z1)
(visited x3y0z2)
(visited x3y0z3)
(visited x3y0z4)
(visited x3y1z0)
(visited x3y1z1)
(visited x3y1z2)
(visited x3y1z3)
(visited x3y1z4)
(visited x3y2z0)
(visited x3y2z1)
(visited x3y2z2)
(visited x3y2z3)
(visited x3y2z4)
(visited x3y3z0)
(visited x3y3z1)
(visited x3y3z2)
(visited x3y3z3)
(visited x3y3z4)
(visited x3y4z0)
(visited x3y4z1)
(visited x3y4z2)
(visited x3y4z3)
(visited x3y4z4)
(visited x3y5z0)
(visited x3y5z1)
(visited x3y5z2)
(visited x3y5z3)
(visited x3y5z4)
(visited x3y6z0)
(visited x3y6z1)
(visited x3y6z2)
(visited x3y6z3)
(visited x3y6z4)
(visited x3y7z0)
(visited x3y7z1)
(visited x3y7z2)
(visited x3y7z3)
(visited x3y7z4)
(visited x3y8z0)
(visited x3y8z1)
(visited x3y8z2)
(visited x3y8z3)
(visited x3y8z4)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
