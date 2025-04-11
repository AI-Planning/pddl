(define (domain triangle-tire)
  (:requirements :typing :strips :non-deterministic)
  (:types
    location - object
  )
  (:predicates (vehicleat ?loc - location)
	       (spare-in ?loc - location)
	       (road ?from - location ?to - location)
	       (not-flattire))
  (:action move-car
    :parameters (?from - location ?to - location)
    :precondition (and (vehicleat ?from) (road ?from ?to) (not-flattire))
    :effect (and
		 (oneof (and (vehicleat ?to) (not (vehicleat ?from)))
			(and (vehicleat ?to) (not (vehicleat ?from)) (not (not-flattire))))))

  (:action changetire
    :parameters (?loc - location)
    :precondition (and (spare-in ?loc) (vehicleat ?loc))
    :effect (and (not (spare-in ?loc)) (not-flattire))))

