;;Instance with 2x9x1 points
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
x0y8z0 - location
x1y0z0 - location
x1y1z0 - location
x1y2z0 - location
x1y3z0 - location
x1y4z0 - location
x1y5z0 - location
x1y6z0 - location
x1y7z0 - location
x1y8z0 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 2) 
 (= (min_y) 0)  (= (max_y) 9) 
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
(= (xl x0y8z0) 0)
(= (yl x0y8z0) 8)
(= (zl x0y8z0) 0)
(= (xl x1y0z0) 1)
(= (yl x1y0z0) 0)
(= (zl x1y0z0) 0)
(= (xl x1y1z0) 1)
(= (yl x1y1z0) 1)
(= (zl x1y1z0) 0)
(= (xl x1y2z0) 1)
(= (yl x1y2z0) 2)
(= (zl x1y2z0) 0)
(= (xl x1y3z0) 1)
(= (yl x1y3z0) 3)
(= (zl x1y3z0) 0)
(= (xl x1y4z0) 1)
(= (yl x1y4z0) 4)
(= (zl x1y4z0) 0)
(= (xl x1y5z0) 1)
(= (yl x1y5z0) 5)
(= (zl x1y5z0) 0)
(= (xl x1y6z0) 1)
(= (yl x1y6z0) 6)
(= (zl x1y6z0) 0)
(= (xl x1y7z0) 1)
(= (yl x1y7z0) 7)
(= (zl x1y7z0) 0)
(= (xl x1y8z0) 1)
(= (yl x1y8z0) 8)
(= (zl x1y8z0) 0)
(= (battery-level) 25)
(= (battery-level-full) 25)
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
(visited x0y8z0)
(visited x1y0z0)
(visited x1y1z0)
(visited x1y2z0)
(visited x1y3z0)
(visited x1y4z0)
(visited x1y5z0)
(visited x1y6z0)
(visited x1y7z0)
(visited x1y8z0)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
