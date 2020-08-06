;; Inspired by: Rune Jensen & Manuela Veloso 'beam-walk'
;; Author: Miquel Ramirez, July 2014

(define (domain acrobatics)
	(:requirements :typing :strips :non-deterministic)
	(:types location)
	(:predicates
		(up)
		(position ?p - location)
		(next-fwd ?p1 ?p2 - location)
		(next-bwd ?p1 ?p2 - location)
		(ladder-at ?p - location)
		(broken-leg)
	)

	;; Action to move while being on the beam
	(:action walk-on-beam
		:parameters ( ?from - location ?to - location )
		:precondition (and (not (broken-leg)) (up) (position ?from) (next-fwd ?from ?to))
		:effect (oneof
				(and (position ?to) (not (position ?from)))
				(and (not (up)) (position ?to) (not (position ?from)))
			)
	)

	(:action walk-left
		:parameters (?from - location ?to - location)
		:precondition (and (not (broken-leg)) (not (up)) (position ?from) (next-bwd ?from ?to))
		:effect (and (position ?to) (not (position ?from)))
	)

	(:action walk-right
		:parameters (?from - location ?to - location)
		:precondition (and (not (broken-leg)) (not (up)) (position ?from) (next-fwd ?from ?to))
		:effect (and (position ?to) (not (position ?from)))	
	)

	(:action climb
		:parameters (?p - location)
		:precondition (and (not (broken-leg)) (not (up)) (position ?p) (ladder-at ?p))
		:effect (and (up))
	)

	(:action climb-down
		:parameters () 
		:precondition (and (not (broken-leg)) (up))
		:effect (and (not (up)))
	)

	;; effects mean:
	;; 1) agent falls from the beam and breaks its leg
	;; 2) agent falls from the beam, breaks its leg and falls in the middle
	;; 3) agent falls from the beam, does not break its leg and falls in the middle
	;; 4) agent falls from the beam, does not break its leg and falls at destination position
	;; 5) agent falls from the beam, breaks its leg and fall at destination position
	;; 6) agent falls on the beam at the destination position
	
	(:action jump-over
		:parameters ( ?from - location ?middle - location ?to - location )
		:precondition (and (not (broken-leg)) (up) (position ?from) (next-fwd ?from ?middle) (next-fwd ?middle ?to))
		:effect (oneof
				(and (not (up)) (broken-leg)) 
				(and (not (up)) (broken-leg) (position ?middle) (not (position ?from)))
				(and (not (up)) (position ?middle) (not (position ?from)))
				(and (not (up)) (broken-leg) (position ?to) (not (position ?from)))
				(and (not (up)) (position ?to) (not (position ?from)))
				(and (position ?to) (not (position ?from)))
			)
	)
)
