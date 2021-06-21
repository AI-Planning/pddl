(define (problem miner-0)
(:domain miner)
(:objects 
	L11 - location
	L12 - location
	L21 - location
	L22 - location
	L31 - location
	L32 - location
	L41 - location
	L42 - location
	L51 - location
	L52 - location
	L61 - location
	L62 - location
	L71 - location
	L72 - location
	L81 - location
	L82 - location

	r1 - rock
	r2 - rock
	r3 - rock
	r4 - rock
	r5 - rock
	r6 - rock
)
(:init
	(person-alive)
	(person-at L11)
	(goldcount-0)
	(botton-loc L11)

	(rock-at r1 L32)
	(rock-at r2 L22)
	(rock-at r3 L31)
	(rock-at r4 L22)
	(rock-at r5 L32)
	(rock-at r6 L11)

	(gold-bad-at L11)
	(gold-bad-at L31)
	(gold-bad-at L22)

	(gold-good-at L71)
	(gold-good-at L81)
	(gold-good-at L72)

	(road L11 L21)
	(road L11 L12)
	(road L12 L22)
	(road L12 L13)
	(road L12 L11)
	(road L21 L11)
	(road L21 L31)
	(road L21 L22)
	(road L22 L12)
	(road L22 L32)
	(road L22 L23)
	(road L22 L21)
	(road L31 L21)
	(road L31 L41)
	(road L31 L32)
	(road L32 L22)
	(road L32 L42)
	(road L32 L33)
	(road L32 L31)
	(road L41 L31)
	(road L41 L51)
	(road L41 L42)
	(road L42 L32)
	(road L42 L52)
	(road L42 L43)
	(road L42 L41)
	(road L51 L41)
	(road L51 L61)
	(road L51 L52)
	(road L52 L42)
	(road L52 L62)
	(road L52 L53)
	(road L52 L51)
	(road L61 L51)
	(road L61 L71)
	(road L61 L62)
	(road L62 L52)
	(road L62 L72)
	(road L62 L63)
	(road L62 L61)
	(road L71 L61)
	(road L71 L81)
	(road L71 L72)
	(road L72 L62)
	(road L72 L82)
	(road L72 L73)
	(road L72 L71)
	(road L81 L71)
	(road L81 L82)
	(road L82 L72)
	(road L82 L83)
	(road L82 L81)
)
(:goal (and (person-alive) (goldcount-3)))
)