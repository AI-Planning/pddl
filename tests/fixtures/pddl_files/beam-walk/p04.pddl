(define (problem beam-walk-32)
(:domain beam-walk)
(:objects
p0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 - location
)
(:init
(next-fwd p0 p1) (next-fwd p1 p2) (next-fwd p2 p3) (next-fwd p3 p4) (next-fwd p4 p5) (next-fwd p5 p6) (next-fwd p6 p7) (next-fwd p7 p8) (next-fwd p8 p9) (next-fwd p9 p10) (next-fwd p10 p11) (next-fwd p11 p12) (next-fwd p12 p13) (next-fwd p13 p14) (next-fwd p14 p15) (next-fwd p15 p16) (next-fwd p16 p17) (next-fwd p17 p18) (next-fwd p18 p19) (next-fwd p19 p20) (next-fwd p20 p21) (next-fwd p21 p22) (next-fwd p22 p23) (next-fwd p23 p24) (next-fwd p24 p25) (next-fwd p25 p26) (next-fwd p26 p27) (next-fwd p27 p28) (next-fwd p28 p29) (next-fwd p29 p30) (next-fwd p30 p31)
(next-bwd p1 p0) (next-bwd p2 p1) (next-bwd p3 p2) (next-bwd p4 p3) (next-bwd p5 p4) (next-bwd p6 p5) (next-bwd p7 p6) (next-bwd p8 p7) (next-bwd p9 p8) (next-bwd p10 p9) (next-bwd p11 p10) (next-bwd p12 p11) (next-bwd p13 p12) (next-bwd p14 p13) (next-bwd p15 p14) (next-bwd p16 p15) (next-bwd p17 p16) (next-bwd p18 p17) (next-bwd p19 p18) (next-bwd p20 p19) (next-bwd p21 p20) (next-bwd p22 p21) (next-bwd p23 p22) (next-bwd p24 p23) (next-bwd p25 p24) (next-bwd p26 p25) (next-bwd p27 p26) (next-bwd p28 p27) (next-bwd p29 p28) (next-bwd p30 p29) (next-bwd p31 p30)
(ladder-at p0)
(position p0)
)

(:goal
(and (up) (position p31) )
)

)
