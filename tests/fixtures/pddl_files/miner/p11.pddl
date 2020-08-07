(define (problem miner-0)
(:domain miner)
(:objects 
	L11 - location
	L12 - location
	L13 - location
	L21 - location
	L22 - location
	L23 - location
	L31 - location
	L32 - location
	L33 - location
	L41 - location
	L42 - location
	L43 - location
	L51 - location
	L52 - location
	L53 - location
	L61 - location
	L62 - location
	L63 - location
	L71 - location
	L72 - location
	L73 - location
	L81 - location
	L82 - location
	L83 - location
	L91 - location
	L92 - location
	L93 - location
	L101 - location
	L102 - location
	L103 - location

	r1 - rock
	r2 - rock
	r3 - rock
	r4 - rock
	r5 - rock
	r6 - rock
	r7 - rock
	r8 - rock
	r9 - rock
	r10 - rock
	r11 - rock
	r12 - rock
)
(:init
	(person-alive)
	(person-at L11)
	(goldcount-0)
	(botton-loc L11)

	(rock-at r1 L42)
	(rock-at r2 L22)
	(rock-at r3 L42)
	(rock-at r4 L42)
	(rock-at r5 L23)
	(rock-at r6 L12)
	(rock-at r7 L23)
	(rock-at r8 L22)
	(rock-at r9 L43)
	(rock-at r10 L23)
	(rock-at r11 L41)
	(rock-at r12 L22)

	(gold-bad-at L11)
	(gold-bad-at L21)
	(gold-bad-at L33)
	(gold-bad-at L43)

	(gold-good-at L81)
	(gold-good-at L83)
	(gold-good-at L101)
	(gold-good-at L92)

	(road L11 L21)
	(road L11 L12)
	(road L12 L22)
	(road L12 L13)
	(road L12 L11)
	(road L13 L23)
	(road L13 L14)
	(road L13 L12)
	(road L21 L11)
	(road L21 L31)
	(road L21 L22)
	(road L22 L12)
	(road L22 L32)
	(road L22 L23)
	(road L22 L21)
	(road L23 L13)
	(road L23 L33)
	(road L23 L24)
	(road L23 L22)
	(road L31 L21)
	(road L31 L41)
	(road L31 L32)
	(road L32 L22)
	(road L32 L42)
	(road L32 L33)
	(road L32 L31)
	(road L33 L23)
	(road L33 L43)
	(road L33 L34)
	(road L33 L32)
	(road L41 L31)
	(road L41 L51)
	(road L41 L42)
	(road L42 L32)
	(road L42 L52)
	(road L42 L43)
	(road L42 L41)
	(road L43 L33)
	(road L43 L53)
	(road L43 L44)
	(road L43 L42)
	(road L51 L41)
	(road L51 L61)
	(road L51 L52)
	(road L52 L42)
	(road L52 L62)
	(road L52 L53)
	(road L52 L51)
	(road L53 L43)
	(road L53 L63)
	(road L53 L54)
	(road L53 L52)
	(road L61 L51)
	(road L61 L71)
	(road L61 L62)
	(road L62 L52)
	(road L62 L72)
	(road L62 L63)
	(road L62 L61)
	(road L63 L53)
	(road L63 L73)
	(road L63 L64)
	(road L63 L62)
	(road L71 L61)
	(road L71 L81)
	(road L71 L72)
	(road L72 L62)
	(road L72 L82)
	(road L72 L73)
	(road L72 L71)
	(road L73 L63)
	(road L73 L83)
	(road L73 L74)
	(road L73 L72)
	(road L81 L71)
	(road L81 L91)
	(road L81 L82)
	(road L82 L72)
	(road L82 L92)
	(road L82 L83)
	(road L82 L81)
	(road L83 L73)
	(road L83 L93)
	(road L83 L84)
	(road L83 L82)
	(road L91 L81)
	(road L91 L101)
	(road L91 L92)
	(road L92 L82)
	(road L92 L102)
	(road L92 L93)
	(road L92 L91)
	(road L93 L83)
	(road L93 L103)
	(road L93 L94)
	(road L93 L92)
	(road L101 L91)
	(road L101 L102)
	(road L102 L92)
	(road L102 L103)
	(road L102 L101)
	(road L103 L93)
	(road L103 L104)
	(road L103 L102)
)
(:goal (and (person-alive) (goldcount-3)))
)