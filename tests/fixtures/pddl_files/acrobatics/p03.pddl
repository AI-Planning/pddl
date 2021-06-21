(define (problem acrobatics-8)
(:domain acrobatics)
(:objects
p0 p1 p2 p3 p4 p5 p6 p7 - location
)
(:init
(next-fwd p0 p1) (next-fwd p1 p2) (next-fwd p2 p3) (next-fwd p3 p4) (next-fwd p4 p5) (next-fwd p5 p6) (next-fwd p6 p7)
(next-bwd p1 p0) (next-bwd p2 p1) (next-bwd p3 p2) (next-bwd p4 p3) (next-bwd p5 p4) (next-bwd p6 p5) (next-bwd p7 p6)
(ladder-at p0)
(position p0)
)

(:goal
(and (up) (position p7) )
)

)
