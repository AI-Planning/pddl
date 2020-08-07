(define (problem doors-0)
(:domain doors)
(:objects
L1 - location
L2 - location
L3 - location
L4 - location
L5 - location
L6 - location
L7 - location
L8 - location
L9 - location
L10 - location
L11 - location
L12 - location
L13 - location
L14 - location
L15 - location
L16 - location
D2 - door
D3 - door
D4 - door
D5 - door
D6 - door
D7 - door
D8 - door
D9 - door
D10 - door
D11 - door
D12 - door
D13 - door
D14 - door
D15 - door
D16 - door
)
(:init
(player-at L1)
(initial-location L1)
(open D2)
(open D3)
(open D4)
(open D5)
(open D6)
(open D7)
(open D8)
(open D9)
(open D10)
(open D11)
(open D12)
(open D13)
(open D14)
(open D15)
(open D16)
(door-in D2 L2)
(door-in D3 L3)
(door-in D4 L4)
(door-in D5 L5)
(door-in D6 L6)
(door-in D7 L7)
(door-in D8 L8)
(door-in D9 L9)
(door-in D10 L10)
(door-in D11 L11)
(door-in D12 L12)
(door-in D13 L13)
(door-in D14 L14)
(door-in D15 L15)
(door-in D16 L16)
(door-out D2 L1)
(door-out D3 L2)
(door-out D4 L3)
(door-out D5 L4)
(door-out D6 L5)
(door-out D7 L6)
(door-out D8 L7)
(door-out D9 L8)
(door-out D10 L9)
(door-out D11 L10)
(door-out D12 L11)
(door-out D13 L12)
(door-out D14 L13)
(door-out D15 L14)
(door-out D16 L15)
(final-location L16)
)
(:goal (player-at L16)))