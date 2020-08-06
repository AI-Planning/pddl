(define (problem islands-0)
(:domain islands)
(:objects 
	L11-1 - location
	L12-1 - location
	L21-1 - location
	L22-1 - location

	L11-2 - location
	L12-2 - location
	L21-2 - location
	L22-2 - location

)
(:init
	(person-alive)
	(person-at L22-1)
	(bridge-clear)
	; If some monkey initially in bridge change this!

	(bridge-drop-location L11-1)
	(bridge-drop-location L11-2)

	(swim-road L12-1 L21-2) (swim-road L21-2 L12-1)
	(swim-road L22-1 L21-2) (swim-road L21-2 L22-1)

	(bridge-road L11-1 L12-2) (bridge-road L12-2 L11-1)
	(bridge-road L21-1 L22-2) (bridge-road L22-2 L21-1)

	(road L11-1 L21-1)
	(road L11-1 L12-1)
	(road L12-1 L22-1)
	(road L12-1 L11-1)
	(road L21-1 L11-1)
	(road L21-1 L22-1)
	(road L22-1 L12-1)
	(road L22-1 L21-1)

	(road L11-2 L21-2)
	(road L11-2 L12-2)
	(road L12-2 L22-2)
	(road L12-2 L11-2)
	(road L21-2 L11-2)
	(road L21-2 L22-2)
	(road L22-2 L12-2)
	(road L22-2 L21-2)

	;Change monkeys location at will
)
(:goal (person-at L21-2))
)