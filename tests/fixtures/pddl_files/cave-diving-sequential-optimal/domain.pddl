;; Cave Diving ADL
;; Authors: Nathan Robinson,
;;          Christian Muise, and
;;          Charles Gretton

(define (domain cave-diving-adl)
  (:requirements :typing :action-costs :adl :numeric-fluents :action-costs)
  (:types
       location diver tank quantity - object
  )
  (:predicates
    (at-tank ?t - tank ?l - location)
    (in-storage ?t - tank)
    (full ?t - tank)
    (next-tank ?t1 - tank ?t2 - tank)
    (at-diver ?d - diver ?l - location)
    (available ?d - diver)
    (at-surface ?d - diver)
    (decompressing ?d - diver)
    (precludes ?d1 - diver ?d2 - diver)
    (cave-entrance ?l - location)
    (connected ?l1 - location ?l2 - location)
    (next-quantity ?q1 - quantity ?q2 - quantity)
    (holding ?d - diver ?t - tank)
    (capacity ?d - diver ?q - quantity)
    (have-photo ?l - location)
    (in-water )
  )

  (:functions
    (hiring-cost ?d - diver) - number
    (other-cost) - number
    (total-cost) - number
  )

  (:action hire-diver
    :parameters (?d1 - diver)
    :precondition (and      (available ?d1)
                       (not (in-water))
                  )
    :effect (and (at-surface ?d1)
                 (not (available ?d1))
                 (forall (?d2 - diver)
                     (when (precludes ?d1 ?d2) (not (available ?d2))))
                 (in-water)
                 (increase (total-cost) (hiring-cost ?d1))
            )
  )

  (:action prepare-tank
    :parameters (?d - diver ?t1 ?t2 - tank ?q1 ?q2 - quantity)
    :precondition (and (at-surface ?d)
                       (in-storage ?t1)
                       (next-quantity ?q1 ?q2)
                       (capacity ?d ?q2)
                       (next-tank ?t1 ?t2)
                  )
    :effect (and (not (in-storage ?t1))
                 (not (capacity ?d ?q2))
                      (in-storage ?t2)
                      (full ?t1)
                      (capacity ?d ?q1)
                      (holding ?d ?t1)
                 (increase (total-cost) (other-cost ))
            )
  )

  (:action enter-water
    :parameters (?d - diver ?l - location)
    :precondition (and (at-surface ?d)
                       (cave-entrance ?l)
                  )
    :effect (and (not (at-surface ?d))
                      (at-diver ?d ?l)
                 (increase (total-cost) (other-cost ))
            )
  )

  (:action pickup-tank
    :parameters (?d - diver ?t - tank ?l - location ?q1 ?q2 - quantity)
    :precondition (and (at-diver ?d ?l)
                       (at-tank ?t ?l)
                       (next-quantity ?q1 ?q2)
                       (capacity ?d ?q2)
                  )
    :effect (and (not (at-tank ?t ?l))
                 (not (capacity ?d ?q2))
                      (holding ?d ?t)
                      (capacity ?d ?q1)
                 (increase (total-cost) (other-cost ))
            )
  )

  (:action drop-tank
    :parameters (?d - diver ?t - tank ?l - location ?q1 ?q2 - quantity)
    :precondition (and (at-diver ?d ?l)
                       (holding ?d ?t)
                       (next-quantity ?q1 ?q2)
                       (capacity ?d ?q1)
                  )
    :effect (and (not (holding ?d ?t))
                 (not (capacity ?d ?q1))
                      (at-tank ?t ?l)
                      (capacity ?d ?q2)
                 (increase (total-cost) (other-cost ))
            )
  )

  (:action swim
    :parameters (?d - diver ?t - tank ?l1 ?l2 - location)
    :precondition (and (at-diver ?d ?l1)
                       (holding ?d ?t)
                       (full ?t)
                       (connected ?l1 ?l2)
                  )
    :effect (and (not (at-diver ?d ?l1))
                 (not (full ?t))
                      (at-diver ?d ?l2)
                 (increase (total-cost) (other-cost ))
            )
  )

  (:action photograph
    :parameters (?d - diver ?l - location ?t - tank)
    :precondition (and (at-diver ?d ?l)
                       (holding ?d ?t)
                       (full ?t)
                  )
    :effect (and (not (full ?t))
                      (have-photo ?l)
                 (increase (total-cost) (other-cost ))
            )
  )

  (:action decompress
    :parameters (?d - diver ?l - location)
    :precondition (and (at-diver ?d ?l)
                       (cave-entrance ?l)
                  )
    :effect (and (not (at-diver ?d ?l))
                      (decompressing ?d)
                 (not (in-water))
                 (increase (total-cost) (other-cost ))
            )
  )

)
