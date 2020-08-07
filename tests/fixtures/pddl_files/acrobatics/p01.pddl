(define (problem beam-walk-2)
(:domain acrobatics)
(:objects
p0 p1 - location
)
(:init
(next-fwd p0 p1)
(next-bwd p1 p0)
(ladder-at p0)
(position p0)
)

(:goal
(and (up) (position p1) )
)

)
