(define (problem doors-0)
(:domain doors)
(:objects
L1 - location
L2 - location
L3 - location
D2 - door
D3 - door
)
(:init
(player-at L1)
(initial-location L1)
(open D2)
(open D3)
(door-in D2 L2)
(door-in D3 L3)
(door-out D2 L1)
(door-out D3 L2)
(final-location L3)
)
(:goal (player-at L3)))