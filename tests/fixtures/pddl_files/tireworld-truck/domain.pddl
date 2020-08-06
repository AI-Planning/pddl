(define (domain tire-truck)
  (:requirements :typing :strips :non-deterministic :negative-preconditions)
  (:types location tire)
  (:predicates (car-at ?loc - location)
               (truck-at ?loc - location)
               (tire-at ?t - tire ?loc - location)
               (car-road ?from - location ?to - location)
               (car-spiky-road ?from - location ?to - location)
               (truck-road ?from - location ?to - location)
               (not-flattire)
               (loaded ?t - tire)
               (not-hasspare)
               (initial-location ?loc)
               (free ?loc))


  (:action move-car-spiky
    :parameters (?from - location ?to - location)
    :precondition (and (car-at ?from) (free ?to) (not (initial-location ?to)) (car-spiky-road ?from ?to) (not-flattire))
    :effect (and (car-at ?to) (free ?from) (not (free ?to)) (not (car-at ?from)) (oneof (and) (and (not (not-flattire)))))
  )

  (:action move-car-spiky-to-initial
    :parameters (?from - location ?to - location)
    :precondition (and (initial-location ?to) (car-at ?from) (car-spiky-road ?from ?to) (not-flattire))
    :effect (and (car-at ?to) (free ?from) (free ?to) (not (car-at ?from)) (oneof (and) (and (not (not-flattire)))))
  )


  (:action move-car-normal
    :parameters (?from - location ?to - location)
    :precondition (and (car-at ?from) (free ?to) (not (initial-location ?to)) (car-road ?from ?to) (not-flattire))
    :effect (and (car-at ?to) (free ?from) (not (free ?to)) (not (car-at ?from)))
  )

  (:action move-car-normal-to-initial
    :parameters (?from - location ?to - location)
    :precondition (and (initial-location ?to) (car-at ?from) (car-road ?from ?to) (not-flattire))
    :effect (and (car-at ?to) (free ?from) (free ?to) (not (car-at ?from)))
  )


  (:action move-truck
    :parameters (?from - location ?to - location)
    :precondition (and (truck-at ?from) (free ?to) (not (initial-location ?to)) (truck-road ?from ?to))
    :effect (and (truck-at ?to) (free ?from) (not (free ?to)) (not (truck-at ?from)))
  )

  (:action move-truck-to-initial
    :parameters (?from - location ?to - location)
    :precondition (and (initial-location ?to) (truck-at ?from) (truck-road ?from ?to))
    :effect (and (truck-at ?to) (free ?from) (free ?to) (not (truck-at ?from)))
  )


  (:action loadtire
    :parameters (?loc - location ?t - tire)
    :precondition (and (truck-at ?loc) (tire-at ?t ?loc))
    :effect (and (loaded ?t) (not (tire-at ?t ?loc)))
  )

  (:action droptire
    :parameters (?loc - location ?t - tire)
    :precondition (and (truck-at ?loc) (loaded ?t))
    :effect (and (not (loaded ?t)) (tire-at ?t ?loc))
  )

  (:action fix
    :parameters (?loc - location ?t - tire)
    :precondition (and (car-at ?loc) (not (not-flattire)) (tire-at ?t ?loc))
    :effect (and (not-flattire) (not (tire-at ?t ?loc)))
  )

)