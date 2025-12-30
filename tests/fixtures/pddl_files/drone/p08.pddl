;;Instance with 8x1x4 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y0z1 - location
x0y0z2 - location
x0y0z3 - location
x1y0z0 - location
x1y0z1 - location
x1y0z2 - location
x1y0z3 - location
x2y0z0 - location
x2y0z1 - location
x2y0z2 - location
x2y0z3 - location
x3y0z0 - location
x3y0z1 - location
x3y0z2 - location
x3y0z3 - location
x4y0z0 - location
x4y0z1 - location
x4y0z2 - location
x4y0z3 - location
x5y0z0 - location
x5y0z1 - location
x5y0z2 - location
x5y0z3 - location
x6y0z0 - location
x6y0z1 - location
x6y0z2 - location
x6y0z3 - location
x7y0z0 - location
x7y0z1 - location
x7y0z2 - location
x7y0z3 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 8) 
 (= (min_y) 0)  (= (max_y) 1) 
 (= (min_z) 0)  (= (max_z) 4) 
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
(= (xl x4y0z0) 4)
(= (yl x4y0z0) 0)
(= (zl x4y0z0) 0)
(= (xl x4y0z1) 4)
(= (yl x4y0z1) 0)
(= (zl x4y0z1) 1)
(= (xl x4y0z2) 4)
(= (yl x4y0z2) 0)
(= (zl x4y0z2) 2)
(= (xl x4y0z3) 4)
(= (yl x4y0z3) 0)
(= (zl x4y0z3) 3)
(= (xl x5y0z0) 5)
(= (yl x5y0z0) 0)
(= (zl x5y0z0) 0)
(= (xl x5y0z1) 5)
(= (yl x5y0z1) 0)
(= (zl x5y0z1) 1)
(= (xl x5y0z2) 5)
(= (yl x5y0z2) 0)
(= (zl x5y0z2) 2)
(= (xl x5y0z3) 5)
(= (yl x5y0z3) 0)
(= (zl x5y0z3) 3)
(= (xl x6y0z0) 6)
(= (yl x6y0z0) 0)
(= (zl x6y0z0) 0)
(= (xl x6y0z1) 6)
(= (yl x6y0z1) 0)
(= (zl x6y0z1) 1)
(= (xl x6y0z2) 6)
(= (yl x6y0z2) 0)
(= (zl x6y0z2) 2)
(= (xl x6y0z3) 6)
(= (yl x6y0z3) 0)
(= (zl x6y0z3) 3)
(= (xl x7y0z0) 7)
(= (yl x7y0z0) 0)
(= (zl x7y0z0) 0)
(= (xl x7y0z1) 7)
(= (yl x7y0z1) 0)
(= (zl x7y0z1) 1)
(= (xl x7y0z2) 7)
(= (yl x7y0z2) 0)
(= (zl x7y0z2) 2)
(= (xl x7y0z3) 7)
(= (yl x7y0z3) 0)
(= (zl x7y0z3) 3)
(= (battery-level) 27)
(= (battery-level-full) 27)
)
(:goal (and 
(visited x0y0z0)
(visited x0y0z1)
(visited x0y0z2)
(visited x0y0z3)
(visited x1y0z0)
(visited x1y0z1)
(visited x1y0z2)
(visited x1y0z3)
(visited x2y0z0)
(visited x2y0z1)
(visited x2y0z2)
(visited x2y0z3)
(visited x3y0z0)
(visited x3y0z1)
(visited x3y0z2)
(visited x3y0z3)
(visited x4y0z0)
(visited x4y0z1)
(visited x4y0z2)
(visited x4y0z3)
(visited x5y0z0)
(visited x5y0z1)
(visited x5y0z2)
(visited x5y0z3)
(visited x6y0z0)
(visited x6y0z1)
(visited x6y0z2)
(visited x6y0z3)
(visited x7y0z0)
(visited x7y0z1)
(visited x7y0z2)
(visited x7y0z3)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
