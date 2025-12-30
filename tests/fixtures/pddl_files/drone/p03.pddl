;;Instance with 1x8x1 points
(define (problem name) (:domain domain_name)
(:objects 
x0y0z0 - location
x0y1z0 - location
x0y2z0 - location
x0y3z0 - location
x0y4z0 - location
x0y5z0 - location
x0y6z0 - location
x0y7z0 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 1) 
 (= (min_y) 0)  (= (max_y) 8) 
 (= (min_z) 0)  (= (max_z) 1) 
(= (xl x0y0z0) 0)
(= (yl x0y0z0) 0)
(= (zl x0y0z0) 0)
(= (xl x0y1z0) 0)
(= (yl x0y1z0) 1)
(= (zl x0y1z0) 0)
(= (xl x0y2z0) 0)
(= (yl x0y2z0) 2)
(= (zl x0y2z0) 0)
(= (xl x0y3z0) 0)
(= (yl x0y3z0) 3)
(= (zl x0y3z0) 0)
(= (xl x0y4z0) 0)
(= (yl x0y4z0) 4)
(= (zl x0y4z0) 0)
(= (xl x0y5z0) 0)
(= (yl x0y5z0) 5)
(= (zl x0y5z0) 0)
(= (xl x0y6z0) 0)
(= (yl x0y6z0) 6)
(= (zl x0y6z0) 0)
(= (xl x0y7z0) 0)
(= (yl x0y7z0) 7)
(= (zl x0y7z0) 0)
(= (battery-level) 21)
(= (battery-level-full) 21)
)
(:goal (and 
(visited x0y0z0)
(visited x0y1z0)
(visited x0y2z0)
(visited x0y3z0)
(visited x0y4z0)
(visited x0y5z0)
(visited x0y6z0)
(visited x0y7z0)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
