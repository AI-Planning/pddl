;;Instance with 1x1x2 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y0z1 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 1) 
 (= (min_y) 0)  (= (max_y) 1) 
 (= (min_z) 0)  (= (max_z) 2) 
(= (xl x0y0z0) 0)
(= (yl x0y0z0) 0)
(= (zl x0y0z0) 0)
(= (xl x0y0z1) 0)
(= (yl x0y0z1) 0)
(= (zl x0y0z1) 1)
(= (battery-level) 9)
(= (battery-level-full) 9)
)
(:goal (and 
(visited x0y0z0)
(visited x0y0z1)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
