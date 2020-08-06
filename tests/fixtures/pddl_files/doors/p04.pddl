(define (problem doors-0)
(:domain doors)
(:objects
L1 - location
L2 - location
L3 - location
L4 - location
L5 - location
L6 - location
D2 - door
D3 - door
D4 - door
D5 - door
D6 - door
)
(:init
(player-at L1)
(initial-location L1)
(open D2)
(open D3)
(open D4)
(open D5)
(open D6)
(door-in D2 L2)
(door-in D3 L3)
(door-in D4 L4)
(door-in D5 L5)
(door-in D6 L6)
(door-out D2 L1)
(door-out D3 L2)
(door-out D4 L3)
(door-out D5 L4)
(door-out D6 L5)
(final-location L6)
)
(:goal (player-at L6)))