(define (problem doors-0)
(:domain doors)
(:objects
L1 - location
L2 - location
L3 - location
L4 - location
D2 - door
D3 - door
D4 - door
)
(:init
(player-at L1)
(initial-location L1)
(open D2)
(open D3)
(open D4)
(door-in D2 L2)
(door-in D3 L3)
(door-in D4 L4)
(door-out D2 L1)
(door-out D3 L2)
(door-out D4 L3)
(final-location L4)
)
(:goal (player-at L4)))