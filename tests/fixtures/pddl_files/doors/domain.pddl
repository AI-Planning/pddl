(define (domain doors)
  (:requirements :typing :strips :non-deterministic :negative-preconditions)
  (:types location door)
  (:predicates (open ?d - door)
               (closed ?d - door)
               (player-at ?loc - location)
               (door-in ?d - door ?loc - location)
               (door-out ?d - door ?loc - location)
               (hold-key)
               (initial-location ?loc - location)
               (final-location ?loc - location))

  (:action pick-key
  	:parameters (?l - location)
  	:precondition (and (player-at ?l) (initial-location ?l))
  	:effect (and (hold-key))
  )

  (:action move-forward-door-open
  	:parameters (?from - location ?to - location ?d1 - door ?d2 - door)
    :precondition (and (player-at ?from) (door-in ?d1 ?to) (open ?d1) (door-out ?d2 ?to) (door-out ?d1 ?from)
    			             (not (final-location ?to)))
    :effect (and (player-at ?to) (not (player-at ?from))
    			       (oneof (and (open ?d1) (not (closed ?d1))) (and (closed ?d1) (not (open ?d1))))
				         (oneof (and (open ?d2) (not (closed ?d2))) (and (closed ?d2) (not (open ?d2)))))
  )

  (:action move-forward-door-closed
  	:parameters (?from - location ?to - location ?d1 - door ?d2 - door)
    :precondition (and (player-at ?from) (door-in ?d1 ?to) (closed ?d1) (door-out ?d2 ?to) (door-out ?d1 ?from)
    			             (not (final-location ?to)))
    :effect (and (player-at ?to) (not (player-at ?from))
    			       (oneof (and (open ?d1) (not (closed ?d1))) (and (closed ?d1) (not (open ?d1))))
				         (oneof (and (open ?d2) (not (closed ?d2))) (and (closed ?d2) (not (open ?d2)))))
  )

  (:action move-forward-last-door-open
  	:parameters (?from - location ?to - location ?d1 - door)
    :precondition (and (player-at ?from) (door-in ?d1 ?to) (open ?d1) (door-out ?d1 ?from)
    			             (final-location ?to))
    :effect (and (player-at ?to) (not (player-at ?from))
    			       (oneof (and (open ?d1) (not (closed ?d1))) (and (closed ?d1) (not (open ?d1)))))
  )

  (:action move-forward-last-door-closed
  	:parameters (?from - location ?to - location ?d1 - door)
    :precondition (and (player-at ?from) (door-in ?d1 ?to) (closed ?d1) (door-out ?d1 ?from)
    			             (final-location ?to) (hold-key))
    :effect (and (player-at ?to) (not (player-at ?from))
    			       (oneof (and (open ?d1) (not (closed ?d1))) (and (closed ?d1) (not (open ?d1)))))
  )
)