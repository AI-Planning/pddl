(define (problem hello-3-times)
    (:domain hello-world-functions)

    (:init
        ; if this was undefined, some planners would not assumed `0`
        (= (hello_counter) 0)
    )

    (:goal
        (>= (hello_counter) 3)
    )
)