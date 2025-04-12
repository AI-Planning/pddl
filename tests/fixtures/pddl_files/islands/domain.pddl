; Domain proposed by Tomas Geffner and Hector Geffner

(define (domain islands)
  (:requirements :typing :strips :non-deterministic)
  (:types
    location monkey - object
  )
  (:predicates (person-at ?loc - location)
               (person-alive)
               (monkey-at ?m - monkey ?loc - location)
               (road ?from - location ?to - location)
               (swim-road ?from - location ?to - location)
               (bridge-road ?from - location ?to - location)
               (bridge-clear)
               (bridge-occupied)
               (monkey-on-bridge ?m - monkey)
               (bridge-drop-location ?loc - location))

  (:action move-person
    :parameters (?from - location ?to - location)
    :precondition (and (person-at ?from) (road ?from ?to) (person-alive))
    :effect (and (person-at ?to) (not (person-at ?from)))
  )

  (:action walk-on-bridge
    :parameters (?from - location ?to - location)
    :precondition (and (person-at ?from) (bridge-road ?from ?to) (bridge-clear) (person-alive))
    :effect (and (not (person-at ?from)) (person-at ?to))
  )

  (:action swim
    :parameters (?from - location ?to - location)
    :precondition (and (person-at ?from) (swim-road ?from ?to) (person-alive))
    :effect (and (not (person-at ?from)) (oneof (person-at ?to) (not (person-alive))))
  )

  (:action move-monkey
    :parameters (?from - location ?to - location ?m - monkey )
    :precondition (and (monkey-at ?m ?from) (road ?from ?to))
    :effect (and (not (monkey-at ?m ?from)) (monkey-at ?m ?to))
  )

  (:action climb-bridge
    :parameters (?m - monkey ?loc - location)
    :precondition (and (bridge-clear) (monkey-at ?m ?loc))
    :effect (and (not (monkey-at ?m ?loc)) (monkey-on-bridge ?m) (not (bridge-clear)) (bridge-occupied))
  )

  (:action leave-bridge
    :parameters (?m - monkey ?loc - location)
    :precondition (and (bridge-occupied) (monkey-on-bridge ?m) (bridge-drop-location ?loc))
    :effect (and (monkey-at ?m ?loc) (not (monkey-on-bridge ?m)) (not (bridge-occupied)) (bridge-clear))
  )

)
