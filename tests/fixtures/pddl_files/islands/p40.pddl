(define (problem islands-0)
(:domain islands)
(:objects 
	L11-1 - location
	L12-1 - location
	L13-1 - location
	L14-1 - location
	L15-1 - location
	L21-1 - location
	L22-1 - location
	L23-1 - location
	L24-1 - location
	L25-1 - location
	L31-1 - location
	L32-1 - location
	L33-1 - location
	L34-1 - location
	L35-1 - location
	L41-1 - location
	L42-1 - location
	L43-1 - location
	L44-1 - location
	L45-1 - location
	L51-1 - location
	L52-1 - location
	L53-1 - location
	L54-1 - location
	L55-1 - location

	L11-2 - location
	L12-2 - location
	L13-2 - location
	L14-2 - location
	L15-2 - location
	L21-2 - location
	L22-2 - location
	L23-2 - location
	L24-2 - location
	L25-2 - location
	L31-2 - location
	L32-2 - location
	L33-2 - location
	L34-2 - location
	L35-2 - location
	L41-2 - location
	L42-2 - location
	L43-2 - location
	L44-2 - location
	L45-2 - location
	L51-2 - location
	L52-2 - location
	L53-2 - location
	L54-2 - location
	L55-2 - location

	m1 - monkey
	m2 - monkey
	m3 - monkey
)
(:init
	(person-alive)
	(person-at L55-1)
	(bridge-clear)
	; If some monkey initially in bridge change this!

	(bridge-drop-location L11-1)
	(bridge-drop-location L11-2)

	(swim-road L15-1 L51-2) (swim-road L51-2 L15-1)
	(swim-road L25-1 L51-2) (swim-road L51-2 L25-1)
	(swim-road L35-1 L51-2) (swim-road L51-2 L35-1)
	(swim-road L45-1 L51-2) (swim-road L51-2 L45-1)
	(swim-road L55-1 L51-2) (swim-road L51-2 L55-1)

	(bridge-road L11-1 L15-2) (bridge-road L15-2 L11-1)
	(bridge-road L21-1 L25-2) (bridge-road L25-2 L21-1)
	(bridge-road L31-1 L35-2) (bridge-road L35-2 L31-1)
	(bridge-road L41-1 L45-2) (bridge-road L45-2 L41-1)
	(bridge-road L51-1 L55-2) (bridge-road L55-2 L51-1)

	(road L11-1 L21-1)
	(road L11-1 L12-1)
	(road L12-1 L22-1)
	(road L12-1 L13-1)
	(road L12-1 L11-1)
	(road L13-1 L23-1)
	(road L13-1 L14-1)
	(road L13-1 L12-1)
	(road L14-1 L24-1)
	(road L14-1 L15-1)
	(road L14-1 L13-1)
	(road L15-1 L25-1)
	(road L15-1 L14-1)
	(road L21-1 L11-1)
	(road L21-1 L31-1)
	(road L21-1 L22-1)
	(road L22-1 L12-1)
	(road L22-1 L32-1)
	(road L22-1 L23-1)
	(road L22-1 L21-1)
	(road L23-1 L13-1)
	(road L23-1 L33-1)
	(road L23-1 L24-1)
	(road L23-1 L22-1)
	(road L24-1 L14-1)
	(road L24-1 L34-1)
	(road L24-1 L25-1)
	(road L24-1 L23-1)
	(road L25-1 L15-1)
	(road L25-1 L35-1)
	(road L25-1 L24-1)
	(road L31-1 L21-1)
	(road L31-1 L41-1)
	(road L31-1 L32-1)
	(road L32-1 L22-1)
	(road L32-1 L42-1)
	(road L32-1 L33-1)
	(road L32-1 L31-1)
	(road L33-1 L23-1)
	(road L33-1 L43-1)
	(road L33-1 L34-1)
	(road L33-1 L32-1)
	(road L34-1 L24-1)
	(road L34-1 L44-1)
	(road L34-1 L35-1)
	(road L34-1 L33-1)
	(road L35-1 L25-1)
	(road L35-1 L45-1)
	(road L35-1 L34-1)
	(road L41-1 L31-1)
	(road L41-1 L51-1)
	(road L41-1 L42-1)
	(road L42-1 L32-1)
	(road L42-1 L52-1)
	(road L42-1 L43-1)
	(road L42-1 L41-1)
	(road L43-1 L33-1)
	(road L43-1 L53-1)
	(road L43-1 L44-1)
	(road L43-1 L42-1)
	(road L44-1 L34-1)
	(road L44-1 L54-1)
	(road L44-1 L45-1)
	(road L44-1 L43-1)
	(road L45-1 L35-1)
	(road L45-1 L55-1)
	(road L45-1 L44-1)
	(road L51-1 L41-1)
	(road L51-1 L52-1)
	(road L52-1 L42-1)
	(road L52-1 L53-1)
	(road L52-1 L51-1)
	(road L53-1 L43-1)
	(road L53-1 L54-1)
	(road L53-1 L52-1)
	(road L54-1 L44-1)
	(road L54-1 L55-1)
	(road L54-1 L53-1)
	(road L55-1 L45-1)
	(road L55-1 L54-1)

	(road L11-2 L21-2)
	(road L11-2 L12-2)
	(road L12-2 L22-2)
	(road L12-2 L13-2)
	(road L12-2 L11-2)
	(road L13-2 L23-2)
	(road L13-2 L14-2)
	(road L13-2 L12-2)
	(road L14-2 L24-2)
	(road L14-2 L15-2)
	(road L14-2 L13-2)
	(road L15-2 L25-2)
	(road L15-2 L14-2)
	(road L21-2 L11-2)
	(road L21-2 L31-2)
	(road L21-2 L22-2)
	(road L22-2 L12-2)
	(road L22-2 L32-2)
	(road L22-2 L23-2)
	(road L22-2 L21-2)
	(road L23-2 L13-2)
	(road L23-2 L33-2)
	(road L23-2 L24-2)
	(road L23-2 L22-2)
	(road L24-2 L14-2)
	(road L24-2 L34-2)
	(road L24-2 L25-2)
	(road L24-2 L23-2)
	(road L25-2 L15-2)
	(road L25-2 L35-2)
	(road L25-2 L24-2)
	(road L31-2 L21-2)
	(road L31-2 L41-2)
	(road L31-2 L32-2)
	(road L32-2 L22-2)
	(road L32-2 L42-2)
	(road L32-2 L33-2)
	(road L32-2 L31-2)
	(road L33-2 L23-2)
	(road L33-2 L43-2)
	(road L33-2 L34-2)
	(road L33-2 L32-2)
	(road L34-2 L24-2)
	(road L34-2 L44-2)
	(road L34-2 L35-2)
	(road L34-2 L33-2)
	(road L35-2 L25-2)
	(road L35-2 L45-2)
	(road L35-2 L34-2)
	(road L41-2 L31-2)
	(road L41-2 L51-2)
	(road L41-2 L42-2)
	(road L42-2 L32-2)
	(road L42-2 L52-2)
	(road L42-2 L43-2)
	(road L42-2 L41-2)
	(road L43-2 L33-2)
	(road L43-2 L53-2)
	(road L43-2 L44-2)
	(road L43-2 L42-2)
	(road L44-2 L34-2)
	(road L44-2 L54-2)
	(road L44-2 L45-2)
	(road L44-2 L43-2)
	(road L45-2 L35-2)
	(road L45-2 L55-2)
	(road L45-2 L44-2)
	(road L51-2 L41-2)
	(road L51-2 L52-2)
	(road L52-2 L42-2)
	(road L52-2 L53-2)
	(road L52-2 L51-2)
	(road L53-2 L43-2)
	(road L53-2 L54-2)
	(road L53-2 L52-2)
	(road L54-2 L44-2)
	(road L54-2 L55-2)
	(road L54-2 L53-2)
	(road L55-2 L45-2)
	(road L55-2 L54-2)

	(monkey-at m1 L23-2)
	(monkey-at m2 L52-2)
	(monkey-at m3 L42-1)
	;Change monkeys location at will
)
(:goal (person-at L51-2))
)