;;;  Authors: Michael Littman and David Weissman  ;;;
;;;  Modified: Blai Bonet for IPC 2006 ;;;

;;;  Modified: Christian Muise to make it a FOND domain

;;;

(define (domain sptire)
  (:requirements :typing :strips :non-deterministic)
  (:types
    location tire - object
  )
  (:predicates (vehicle-at ?loc - location)
               (tire-at ?t - tire ?loc - location)
               (road ?from - location ?to - location)
               (spiky_road ?from - location ?to - location)
               (not-flattire)
               (flattire)
               (hasspare ?t - tire)
               (not-hasspare))

  (:action move-car-spiky
    :parameters (?from - location ?to - location)
    :precondition (and (vehicle-at ?from) (spiky-road ?from ?to) (not-flattire))
    :effect (and (vehicle-at ?to) (not (vehicle-at ?from)) (oneof (and) (and (not (not-flattire)) (flattire))))
  )

  (:action move-car-normal
    :parameters (?from - location ?to - location)
    :precondition (and (vehicle-at ?from) (road ?from ?to) (not-flattire))
    :effect (and (vehicle-at ?to) (not (vehicle-at ?from)))
  )

  (:action loadtire
    :parameters (?loc - location ?t - tire)
    :precondition (and (vehicle-at ?loc) (tire-at ?t ?loc) (not-hasspare))
    :effect (and (hasspare ?t) (not (not-hasspare)) (not (tire-at ?t ?loc)))
  )

  (:action droptire
    :parameters (?loc - location ?t - tire)
    :precondition (and (vehicle-at ?loc) (hasspare ?t))
    :effect (and (not (hasspare ?t)) (not-hasspare) (tire-at ?t ?loc))
  )

  (:action fix-1
    :parameters (?t - tire)
    :precondition (and (hasspare ?t) (flattire))
    :effect (and (not-hasspare) (not-flattire) (not (flattire)) (not (hasspare ?t)))
  )

  ;(:action fix-2
  ;  :parameters (?t - tire ?loc - location)
  ;  ;:precondition (and (tire-at ?t ?loc) (flattire) (vehicle-at ?loc))
  ;  :precondition (and (tire-at ?t ?loc) (vehicle-at ?loc))
  ;  :effect (and (not-flattire) (not (flattire)) (not (tire-at ?t ?loc)))
  ;)

)
