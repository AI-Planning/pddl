(define (problem beam-walk-4)
(:domain beam-walk)
(:objects
p0 p1 p2 p3 - location
)
(:init
(next-fwd p0 p1) (next-fwd p1 p2) (next-fwd p2 p3)
(next-bwd p1 p0) (next-bwd p2 p1) (next-bwd p3 p2)
(ladder-at p0)
(position p0)
)

(:goal
(and (up) (position p3) )
)

)
