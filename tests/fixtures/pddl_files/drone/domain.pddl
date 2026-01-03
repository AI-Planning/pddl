    ;; Enrico Scala <enricos83@gmail.com>
    
    ;; This is a domain modeling a 3D-located UAV whose task is to visit a certain number of locations whose description
    ;; is also given in terms of their exact 3D position. A challenging aspect here is to account for the battery
    ;; since the UAV cannot do all visit at once but needs to recharge at the starting location. Therefore, multiple
    ;; travels back and forth may be necessary to cover visit all locations.
    
    (define
    (domain domain_name)
    (:requirements :strips :typing :numeric-fluents)

    (:types 
           location - object
    )



    (:predicates 
        (visited ?x - location)
    )
    (:functions
        (x)
        (y)
        (z) 
        (xl ?l - location)
        (yl ?l - location)
        (zl ?l - location)
        (battery-level)
        (battery-level-full)        
        (min_x)
        (max_x)
        (min_y)
        (max_y)
        (min_z)
        (max_z)
    )

    (:action increase_x
        :parameters ()
        :precondition (and 
                          (>= (battery-level) 1)
                          (<= (+ (x) 1) (max_x) )
                      )
        :effect (and (increase (x) 1) 
                    (decrease (battery-level) 1)
                )
    )

    (:action decrease_x
        :parameters ()
        :precondition (and 
                            (>= (battery-level) 1)
                            (>= (x) (+ (min_x) 1) )
                      )
        :effect (and (decrease (x) 1)
                    (decrease (battery-level) 1)
                )
    )


    (:action increase_y
        :parameters ()
        :precondition (and 
                            (>= (battery-level) 1)
                            (<= (+ (y) 1) (max_y) )
                      )
        :effect (and (increase (y) 1)
                    (decrease (battery-level) 1)
                )
    )
    (:action decrease_y
        :parameters ()
        :precondition (and 
                            (>= (battery-level) 1)
                            (>= (y) (+ (min_y) 1) )
                      )
        :effect (and (decrease (y) 1)
                    (decrease (battery-level) 1)
                )
    )


    (:action increase_z
        :parameters ()
        :precondition (and 
                            (>= (battery-level) 1)
                            (<= (+ (z) 1) (max_z) )
                      )
        :effect (and (increase (z) 1)
                    (decrease (battery-level) 1)
                )
    )
    (:action decrease_z
        :parameters ()
        :precondition (and 
                            (>= (battery-level) 1)
                            (>= (z) (+ (min_z) 1) )
                      )
        :effect (and (decrease (z) 1)
                    (decrease (battery-level) 1)
                )
    )


    (:action visit
        :parameters (?l - location)
        :precondition (and
                        (>= (battery-level) 1)
                        (= (xl ?l) (x))
                        (= (yl ?l) (y))
                        (= (zl ?l) (z))                        
                       )
        :effect (and (visited ?l)(decrease (battery-level) 1))
    )

    (:action recharge
        :parameters ()
        :precondition (and
                        (= (x) 0)
                        (= (y) 0)
                        (= (z) 0)                        
                       )
        :effect (and 
                       (assign (battery-level) (battery-level-full)))
    )

)
