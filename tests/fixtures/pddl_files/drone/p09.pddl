;;Instance with 3x3x4 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y0z1 - location
x0y0z2 - location
x0y0z3 - location
x0y1z0 - location
x0y1z1 - location
x0y1z2 - location
x0y1z3 - location
x0y2z0 - location
x0y2z1 - location
x0y2z2 - location
x0y2z3 - location
x1y0z0 - location
x1y0z1 - location
x1y0z2 - location
x1y0z3 - location
x1y1z0 - location
x1y1z1 - location
x1y1z2 - location
x1y1z3 - location
x1y2z0 - location
x1y2z1 - location
x1y2z2 - location
x1y2z3 - location
x2y0z0 - location
x2y0z1 - location
x2y0z2 - location
x2y0z3 - location
x2y1z0 - location
x2y1z1 - location
x2y1z2 - location
x2y1z3 - location
x2y2z0 - location
x2y2z1 - location
x2y2z2 - location
x2y2z3 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 3) 
 (= (min_y) 0)  (= (max_y) 3) 
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
(= (battery-level) 21)
(= (battery-level-full) 21)
)
(:goal (and 
(visited x0y0z0)
(visited x0y0z1)
(visited x0y0z2)
(visited x0y0z3)
(visited x0y1z0)
(visited x0y1z1)
(visited x0y1z2)
(visited x0y1z3)
(visited x0y2z0)
(visited x0y2z1)
(visited x0y2z2)
(visited x0y2z3)
(visited x1y0z0)
(visited x1y0z1)
(visited x1y0z2)
(visited x1y0z3)
(visited x1y1z0)
(visited x1y1z1)
(visited x1y1z2)
(visited x1y1z3)
(visited x1y2z0)
(visited x1y2z1)
(visited x1y2z2)
(visited x1y2z3)
(visited x2y0z0)
(visited x2y0z1)
(visited x2y0z2)
(visited x2y0z3)
(visited x2y1z0)
(visited x2y1z1)
(visited x2y1z2)
(visited x2y1z3)
(visited x2y2z0)
(visited x2y2z1)
(visited x2y2z2)
(visited x2y2z3)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
