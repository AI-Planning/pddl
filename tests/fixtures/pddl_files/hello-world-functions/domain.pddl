;Hello world domain to test numerical fluents (functions) 
(define (domain hello-world-functions)

    (:requirements :strips :numeric-fluents)

    (:functions
        (hello_counter)
    )

    (:action say-hello-world
        :parameters ()
        :precondition (and (<= (hello_counter) 3))
        :effect (and (increase (hello_counter) 1))
    )
)