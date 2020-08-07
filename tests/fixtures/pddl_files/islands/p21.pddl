(define (problem islands-0)
(:domain islands)
(:objects 
	L11-1 - location
	L12-1 - location
	L13-1 - location
	L21-1 - location
	L22-1 - location
	L23-1 - location
	L31-1 - location
	L32-1 - location
	L33-1 - location

	L11-2 - location
	L12-2 - location
	L13-2 - location
	L21-2 - location
	L22-2 - location
	L23-2 - location
	L31-2 - location
	L32-2 - location
	L33-2 - location

	m1 - monkey
	m2 - monkey
	m3 - monkey
	m4 - monkey
	m5 - monkey
	m6 - monkey
	m7 - monkey
	m8 - monkey
)
(:init
	(person-alive)
	(person-at L33-1)
	(bridge-clear)
	; If some monkey initially in bridge change this!

	(bridge-drop-location L11-1)
	(bridge-drop-location L11-2)

	(swim-road L13-1 L31-2) (swim-road L31-2 L13-1)
	(swim-road L23-1 L31-2) (swim-road L31-2 L23-1)
	(swim-road L33-1 L31-2) (swim-road L31-2 L33-1)

	(bridge-road L11-1 L13-2) (bridge-road L13-2 L11-1)
	(bridge-road L21-1 L23-2) (bridge-road L23-2 L21-1)
	(bridge-road L31-1 L33-2) (bridge-road L33-2 L31-1)

	(road L11-1 L21-1)
	(road L11-1 L12-1)
	(road L12-1 L22-1)
	(road L12-1 L13-1)
	(road L12-1 L11-1)
	(road L13-1 L23-1)
	(road L13-1 L12-1)
	(road L21-1 L11-1)
	(road L21-1 L31-1)
	(road L21-1 L22-1)
	(road L22-1 L12-1)
	(road L22-1 L32-1)
	(road L22-1 L23-1)
	(road L22-1 L21-1)
	(road L23-1 L13-1)
	(road L23-1 L33-1)
	(road L23-1 L22-1)
	(road L31-1 L21-1)
	(road L31-1 L32-1)
	(road L32-1 L22-1)
	(road L32-1 L33-1)
	(road L32-1 L31-1)
	(road L33-1 L23-1)
	(road L33-1 L32-1)

	(road L11-2 L21-2)
	(road L11-2 L12-2)
	(road L12-2 L22-2)
	(road L12-2 L13-2)
	(road L12-2 L11-2)
	(road L13-2 L23-2)
	(road L13-2 L12-2)
	(road L21-2 L11-2)
	(road L21-2 L31-2)
	(road L21-2 L22-2)
	(road L22-2 L12-2)
	(road L22-2 L32-2)
	(road L22-2 L23-2)
	(road L22-2 L21-2)
	(road L23-2 L13-2)
	(road L23-2 L33-2)
	(road L23-2 L22-2)
	(road L31-2 L21-2)
	(road L31-2 L32-2)
	(road L32-2 L22-2)
	(road L32-2 L33-2)
	(road L32-2 L31-2)
	(road L33-2 L23-2)
	(road L33-2 L32-2)

	(monkey-at m1 L32-2)
	(monkey-at m2 L21-2)
	(monkey-at m3 L32-1)
	(monkey-at m4 L21-2)
	(monkey-at m5 L33-1)
	(monkey-at m6 L33-1)
	(monkey-at m7 L22-1)
	(monkey-at m8 L13-2)
	;Change monkeys location at will
)
(:goal (person-at L31-2))
)