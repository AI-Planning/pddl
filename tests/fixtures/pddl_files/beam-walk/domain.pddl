;; Authors: Rune Jensen & Manuela Veloso
;; Modified: Rewritten as PDDL by Miquel Ramirez, April 2014

(define (domain beam-walk)
	(:requirements :typing :strips :non-deterministic :negative-preconditions)
	(:types
		location - object
	)
	(:predicates
		(up)
		(position ?p - location)
		(next-fwd ?p1 ?p2 - location)
		(next-bwd ?p1 ?p2 - location)
		(ladder-at ?p - location)
	)

	;; Action to move while being on the beam
	(:action walk-on-beam
		:parameters ( ?from - location ?to - location )
		:precondition (and (up) (position ?from) (next-fwd ?from ?to))
		:effect (oneof
				(and (position ?to) (not (position ?from)))
				(and (not (up)) (position ?to) (not (position ?from)))
			)
	)

	(:action walk
		:parameters (?from - location ?to - location)
		:precondition (and (not (up)) (position ?from) (next-bwd ?from ?to))
		:effect (and (position ?to) (not (position ?from)))
	)

	(:action climb
		:parameters (?p - location)
		:precondition (and (not (up)) (position ?p) (ladder-at ?p))
		:effect (and (up))
	)
)
