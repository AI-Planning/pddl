(define (problem acrobatics-16)
(:domain acrobatics)
(:objects
p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 - location
)
(:init
(next-fwd p0 p1) (next-fwd p1 p2) (next-fwd p2 p3) (next-fwd p3 p4) (next-fwd p4 p5) (next-fwd p5 p6) (next-fwd p6 p7) (next-fwd p7 p8) (next-fwd p8 p9) (next-fwd p9 p10) (next-fwd p10 p11) (next-fwd p11 p12) (next-fwd p12 p13) (next-fwd p13 p14) (next-fwd p14 p15)
(next-bwd p1 p0) (next-bwd p2 p1) (next-bwd p3 p2) (next-bwd p4 p3) (next-bwd p5 p4) (next-bwd p6 p5) (next-bwd p7 p6) (next-bwd p8 p7) (next-bwd p9 p8) (next-bwd p10 p9) (next-bwd p11 p10) (next-bwd p12 p11) (next-bwd p13 p12) (next-bwd p14 p13) (next-bwd p15 p14)
(ladder-at p0)
(position p0)
)

(:goal
(and (up) (position p15) )
)

)
