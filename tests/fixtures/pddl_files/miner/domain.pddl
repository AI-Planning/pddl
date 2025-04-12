; Domain proposed by Tomas Geffner and Hector Geffner

(define (domain miner)
  (:requirements :typing :strips :non-deterministic)
  (:types
    location rock - object
  )
  (:predicates (person-at ?loc - location)
               (botton-loc ?loc - location)
               (person-alive)
               (rock-at ?r - rock ?loc - location)
               (road ?from - location ?to - location)
               (holding ?r - rock)
               (gold-bad-at ?loc - location)
               (gold-good-at ?loc - location)
               (goldcount-0)
               (goldcount-1)
               (goldcount-2)
               (goldcount-3)
               (botton-pressed))

  (:action move-person
    :parameters (?from - location ?to - location)
    :precondition (and (person-at ?from) (road ?from ?to) (person-alive))
    :effect (and (person-at ?to) (not (person-at ?from)))
  )

  (:action pick-rock
    :parameters (?loc - location ?r - rock)
    :precondition (and (person-at ?loc) (rock-at ?r ?loc) (person-alive))
    :effect (and (not (rock-at ?r ?loc)) (holding ?r))
  )

  (:action drop-rock
    :parameters (?loc - location ?r - rock)
    :precondition (and (person-at ?loc) (holding ?r) (person-alive))
    :effect (and (rock-at ?r ?loc) (not (holding ?r)))
  )

  (:action drop-rock-press
    :parameters (?loc - location ?r - rock)
    :precondition (and (person-at ?loc) (holding ?r) (person-alive) (botton-loc ?loc))
    :effect (and (rock-at ?r ?loc) (botton-pressed) (not (holding ?r)))
  )

  (:action pick-bad-gold-1
    :parameters (?loc - location)
    :precondition (and (person-at ?loc) (gold-bad-at ?loc) (person-alive) (goldcount-0))
    :effect (oneof (not (person-alive))
    			   (and (not (goldcount-0)) (goldcount-1) (not (gold-bad-at ?loc))))
  )

  (:action pick-bad-gold-2
    :parameters (?loc - location)
    :precondition (and (person-at ?loc) (gold-bad-at ?loc) (person-alive) (goldcount-1))
    :effect (oneof (not (person-alive))
    			   (and (not (goldcount-1)) (goldcount-2) (not (gold-bad-at ?loc))))
  )

  (:action pick-bad-gold-3
    :parameters (?loc - location)
    :precondition (and (person-at ?loc) (gold-bad-at ?loc) (person-alive) (goldcount-2))
    :effect (oneof (not (person-alive))
    			   (and (not (goldcount-2)) (goldcount-3) (not (gold-bad-at ?loc))))
  )

  (:action pick-good-gold-1
    :parameters (?loc - location)
    :precondition (and (person-at ?loc) (gold-good-at ?loc) (person-alive) (goldcount-0) (botton-pressed))
    :effect (and (not (goldcount-0)) (goldcount-1) (not (gold-good-at ?loc)))
  )

  (:action pick-good-gold-2
    :parameters (?loc - location)
    :precondition (and (person-at ?loc) (gold-good-at ?loc) (person-alive) (goldcount-1) (botton-pressed))
    :effect (and (not (goldcount-1)) (goldcount-2) (not (gold-good-at ?loc)))
  )

  (:action pick-good-gold-3
    :parameters (?loc - location)
    :precondition (and (person-at ?loc) (gold-good-at ?loc) (person-alive) (goldcount-2) (botton-pressed))
    :effect (and (not (goldcount-2)) (goldcount-3) (not (gold-good-at ?loc)))
  )

)
