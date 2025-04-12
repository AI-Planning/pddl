;;;  Authors: Michael Littman and David Weissman  ;;;
;;;  Modified: Blai Bonet for IPC 2006 ;;;

;;;  Modified: Christian Muise to make it a FOND domain
;;;  Modified: Rewritten for ADL, variables modified by effects appear in precondition

(define (domain tire-adl)
  (:requirements :typing :strips :non-deterministic :negative-preconditions)
  (:types
    location - object
  )
  (:predicates (vehicle-at ?loc - location) (spare-in ?loc - location) (road ?from - location ?to - location) (flattire) (hasspare))

  ;; Two (and) effects give us 1/3 probability of getting a flat -- the original probability was 2/5
  (:action move-car
    :parameters (?from - location ?to - location)
    :precondition (and (vehicle-at ?from) (road ?from ?to) (not (flattire)))
    :effect (oneof 	(and (vehicle-at ?to) (not (vehicle-at ?from)))
		 	(and (vehicle-at ?to) (not (vehicle-at ?from)) (flattire)))
  )

  (:action loadtire
    :parameters (?loc - location)
    :precondition (and (vehicle-at ?loc) (spare-in ?loc) (not (hasspare)))
    :effect (and (hasspare) (not (spare-in ?loc)))
  )

  (:action changetire
    :parameters ()
    :precondition (and (hasspare) (flattire))
    :effect (oneof (and) (and (not (hasspare)) (not (flattire)))) ;; The original domain has a 50% chance of a spare change failing
  )

)

