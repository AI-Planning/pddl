;;Instance with 4x2x5 points
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
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 4) 
 (= (min_y) 0)  (= (max_y) 2) 
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
(= (battery-level) 23)
(= (battery-level-full) 23)
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
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
