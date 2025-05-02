;; Enrico Scala (enricos83@gmail.com) and Dongxu Li (dongxu.li@anu.edu.au,)
;; Reference Paper: Li, D., Scala, E., Haslum, P., & Bogomolov, S. (2018, July). 
;;                  Effect-abstraction based relaxation for linear numeric planning. 
;;                  In Proceedings of the 27th International 
;;                  Joint Conference on Artificial Intelligence (pp. 4787-4793).
;; This is an extended version of the Sailing domain. In this extenstion 
;; boats can accelerate/decelerate to move more/less in a single action. This is obtained by adding
;; a first order control behind the classical moving actions.

(define (domain sailing_ln)
    (:requirements :strips :typing :numeric-fluents)
    (:types
        person
        boat
    )
    (:predicates
        (saved ?t - person)
        (dummy)
    )
    (:functions
        (x ?b - boat)
        (y ?b - boat)
        (v ?b - boat)
        (d ?t - person)
    )

    (:action go_north_east
        :parameters (?b - boat)
        :precondition (and (not (dummy)))
        :effect (and (increase (x ?b) (* (v ?b) 1.5))
            (increase (y ?b) (* (v ?b) 1.5))
        )
    )

    (:action go_north_west
        :parameters (?b - boat)
        :precondition (and (not (dummy)))
        :effect (and (decrease (x ?b) (* (v ?b) 1.5))
            (increase (y ?b) (* (v ?b) 1.5))
        )
    )
    (:action go_est
        :parameters (?b - boat)
        :precondition (and (not (dummy)))
        :effect (and (increase (x ?b) (* (v ?b) 3))
        )
    )
    (:action go_west
        :parameters (?b - boat)
        :precondition (and (not (dummy)))
        :effect (and(decrease (x ?b) (* (v ?b) 3)))
    )
    (:action go_south_west
        :parameters(?b - boat)
        :precondition (and (not (dummy)))
        :effect (and (increase (x ?b) (* (v ?b) 2))
            (decrease (y ?b) (* (v ?b) 2))
        )
    )
    (:action go_south_east
        :parameters(?b - boat)
        :precondition (and (not (dummy)))
        :effect (and (decrease (x ?b) (* (v ?b) 2))
            (decrease (y ?b) (* (v ?b) 2))
        )
    )
    (:action go_south
        :parameters(?b - boat)
        :precondition (and (not (dummy)))
        :effect (and (decrease (y ?b) (* (v ?b) 2)))
    )

    (:action accelerate
        :parameters(?b - boat)
        :precondition (and (<= (+ (v ?b) 1) 3))
        :effect (and (increase (v ?b) 1))
    )

    (:action decelerate
        :parameters(?b - boat)
        :precondition (and (>= (- (v ?b) 1) 1))
        :effect (and (decrease (v ?b) 1))
    )

    (:action save_person
        :parameters(?b - boat ?t - person)
        :precondition ( and (>= (+ (x ?b) (y ?b)) (d ?t))
            (>= (- (y ?b) (x ?b)) (d ?t))
            (<= (+ (x ?b) (y ?b)) (+ (d ?t) 25))
            (<= (- (y ?b) (x ?b)) (+ (d ?t) 25))
            (<= (v ?b) 1)
        )
        :effect (and(saved ?t))
    )

)