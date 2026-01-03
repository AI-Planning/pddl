;;Instance with 1x9x3 points
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
x0y7z0 - location
x0y7z1 - location
x0y7z2 - location
x0y8z0 - location
x0y8z1 - location
x0y8z2 - location
) 
(:init (= (x) 0) (= (y) 0) (= (z) 0)
 (= (min_x) 0)  (= (max_x) 1) 
 (= (min_y) 0)  (= (max_y) 9) 
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
(= (xl x0y7z0) 0)
(= (yl x0y7z0) 7)
(= (zl x0y7z0) 0)
(= (xl x0y7z1) 0)
(= (yl x0y7z1) 7)
(= (zl x0y7z1) 1)
(= (xl x0y7z2) 0)
(= (yl x0y7z2) 7)
(= (zl x0y7z2) 2)
(= (xl x0y8z0) 0)
(= (yl x0y8z0) 8)
(= (zl x0y8z0) 0)
(= (xl x0y8z1) 0)
(= (yl x0y8z1) 8)
(= (zl x0y8z1) 1)
(= (xl x0y8z2) 0)
(= (yl x0y8z2) 8)
(= (zl x0y8z2) 2)
(= (battery-level) 27)
(= (battery-level-full) 27)
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
(visited x0y7z0)
(visited x0y7z1)
(visited x0y7z2)
(visited x0y8z0)
(visited x0y8z1)
(visited x0y8z2)
(= (x) 0) (= (y) 0) (= (z) 0) ))
);; end of the problem instance
