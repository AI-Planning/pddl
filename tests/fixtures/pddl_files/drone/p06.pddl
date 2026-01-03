;;Instance with 5x2x2 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y0z1 - location
x0y1z0 - location
x0y1z1 - location
x1y0z0 - location
x1y0z1 - location
x1y1z0 - location
x1y1z1 - location
x2y0z0 - location
x2y0z1 - location
x2y1z0 - location
x2y1z1 - location
x3y0z0 - location
x3y0z1 - location
x3y1z0 - location
x3y1z1 - location
x4y0z0 - location
x4y0z1 - location
x4y1z0 - location
x4y1z1 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 5) 
 (= (min_y) 0)  (= (max_y) 2) 
 (= (min_z) 0)  (= (max_z) 2) 
(= (xl x0y0z0) 0)
(= (yl x0y0z0) 0)
(= (zl x0y0z0) 0)
(= (xl x0y0z1) 0)
(= (yl x0y0z1) 0)
(= (zl x0y0z1) 1)
(= (xl x0y1z0) 0)
(= (yl x0y1z0) 1)
(= (zl x0y1z0) 0)
(= (xl x0y1z1) 0)
(= (yl x0y1z1) 1)
(= (zl x0y1z1) 1)
(= (xl x1y0z0) 1)
(= (yl x1y0z0) 0)
(= (zl x1y0z0) 0)
(= (xl x1y0z1) 1)
(= (yl x1y0z1) 0)
(= (zl x1y0z1) 1)
(= (xl x1y1z0) 1)
(= (yl x1y1z0) 1)
(= (zl x1y1z0) 0)
(= (xl x1y1z1) 1)
(= (yl x1y1z1) 1)
(= (zl x1y1z1) 1)
(= (xl x2y0z0) 2)
(= (yl x2y0z0) 0)
(= (zl x2y0z0) 0)
(= (xl x2y0z1) 2)
(= (yl x2y0z1) 0)
(= (zl x2y0z1) 1)
(= (xl x2y1z0) 2)
(= (yl x2y1z0) 1)
(= (zl x2y1z0) 0)
(= (xl x2y1z1) 2)
(= (yl x2y1z1) 1)
(= (zl x2y1z1) 1)
(= (xl x3y0z0) 3)
(= (yl x3y0z0) 0)
(= (zl x3y0z0) 0)
(= (xl x3y0z1) 3)
(= (yl x3y0z1) 0)
(= (zl x3y0z1) 1)
(= (xl x3y1z0) 3)
(= (yl x3y1z0) 1)
(= (zl x3y1z0) 0)
(= (xl x3y1z1) 3)
(= (yl x3y1z1) 1)
(= (zl x3y1z1) 1)
(= (xl x4y0z0) 4)
(= (yl x4y0z0) 0)
(= (zl x4y0z0) 0)
(= (xl x4y0z1) 4)
(= (yl x4y0z1) 0)
(= (zl x4y0z1) 1)
(= (xl x4y1z0) 4)
(= (yl x4y1z0) 1)
(= (zl x4y1z0) 0)
(= (xl x4y1z1) 4)
(= (yl x4y1z1) 1)
(= (zl x4y1z1) 1)
(= (battery-level) 19)
(= (battery-level-full) 19)
)
(:goal (and 
(visited x0y0z0)
(visited x0y0z1)
(visited x0y1z0)
(visited x0y1z1)
(visited x1y0z0)
(visited x1y0z1)
(visited x1y1z0)
(visited x1y1z1)
(visited x2y0z0)
(visited x2y0z1)
(visited x2y1z0)
(visited x2y1z1)
(visited x3y0z0)
(visited x3y0z1)
(visited x3y1z0)
(visited x3y1z1)
(visited x4y0z0)
(visited x4y0z1)
(visited x4y1z0)
(visited x4y1z1)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
