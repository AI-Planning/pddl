(define (domain earth_observation)
    (:requirements :strips :typing :equality :non-deterministic)

    (:types
            patch - object
            direction - object
            cost-direction - direction
    )

    (:constants
        east - direction
        north-east south-east - cost-direction
    )

    (:predicates 
        (CONNECTED ?p ?n - patch ?d - direction)
        (is-focal-point ?p - patch)
        (is-target ?p - patch)
        (scanned ?p - patch)
    ) 
 
    (:action slew 
     :parameters (?p ?n - patch ?d - cost-direction)
     :precondition 
        (and 
            (CONNECTED ?p ?n ?d)
            (is-focal-point ?p)
        ) 
     :effect 
        (and 
            (increase (total-cost) 1)
            (not (is-focal-point ?p))
            (is-focal-point ?n)
        )
    ) 

    (:action slew
     :parameters (?p ?n - patch)
     :precondition 
        (and 
            (CONNECTED ?p ?n east)
            (is-focal-point ?p)
        ) 
     :effect 
        (and 
            (not (is-focal-point ?p))
            (is-focal-point ?n)
        )
    ) 
 
    (:action take-image
     :parameters (?p ?n - patch)
     :precondition
        (and
            (is-target ?p)
            (is-focal-point ?p)
            (CONNECTED ?p ?n east)
        )
     :effect
        (oneof
            (and
                (not (is-focal-point ?p))
                (not (is-target ?p))
                (is-focal-point ?n)
            )
            (and
                (not (is-focal-point ?p))
                (is-focal-point ?n)
            )
        )
    )
 )

