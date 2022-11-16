(define (domain dom)

(:requirements :strips :non-deterministic :typing)
  (:types
    rover - NO_TYPE
    waypoint - NO_TYPE
    store - NO_TYPE
    camera - NO_TYPE
    mode - NO_TYPE
    lander - NO_TYPE
    objective - NO_TYPE
  )

  (:predicates
    (at ?x0 - rover ?x1 - waypoint)
    (at_lander ?x0 - lander ?x1 - waypoint)
    (can_traverse ?x0 - rover ?x1 - waypoint ?x2 - waypoint)
    (equipped_for_soil_analysis ?x0 - rover)
    (equipped_for_rock_analysis ?x0 - rover)
    (equipped_for_imaging ?x0 - rover)
    (empty ?x0 - store)
    (have_rock_analysis ?x0 - rover ?x1 - waypoint)
    (have_soil_analysis ?x0 - rover ?x1 - waypoint)
    (full ?x0 - store)
    (calibrated ?x0 - camera ?x1 - rover)
    (supports ?x0 - camera ?x1 - mode)
    (available ?x0 - rover)
    (visible ?x0 - waypoint ?x1 - waypoint)
    (have_image ?x0 - rover ?x1 - objective ?x2 - mode)
    (communicatedsoildata ?x0 - waypoint)
    (communicatedrockdata ?x0 - waypoint)
    (communicatedimagedata ?x0 - objective ?x1 - mode)
    (at_soil_sample ?x0 - waypoint)
    (at_rock_sample ?x0 - waypoint)
    (visible_from ?x0 - objective ?x1 - waypoint)
    (store_of ?x0 - store ?x1 - rover)
    (calibration_target ?x0 - camera ?x1 - objective)
    (on_board ?x0 - camera ?x1 - rover)
    (channel_free ?x0 - lander)
  )
  (:action navigate
    :parameters (?x0 - rover ?x1 - waypoint ?x2 - waypoint)
    :precondition 
      (and
        (can_traverse ?x0 ?x1 ?x2)
        (available ?x0)
        (at ?x0 ?x1)
        (visible ?x1 ?x2))
    :effect
      (and
        (at ?x0 ?x2)
        (not 
          (at ?x0 ?x1))
      )
    )
  (:action sample_soil
    :parameters (?x0 - rover ?x1 - store ?x2 - waypoint)
    :precondition 
      (and
        (at ?x0 ?x2)
        (at_soil_sample ?x2)
        (equipped_for_soil_analysis ?x0)
        (store_of ?x1 ?x0)
        (empty ?x1))
    :effect
      (and
        (oneof
          (and
            (full ?x1)
            (have_soil_analysis ?x0 ?x2)
            (not 
              (empty ?x1))
          )
          (and 
            (when (at_rock_sample ?x2)
              (and
                (full ?x1)
                (have_rock_analysis ?x0 ?x2)
                (not 
                  (empty ?x1))
              )
            )
          )
        )
      )
    )
  (:action sample_rock
    :parameters (?x0 - rover ?x1 - store ?x2 - waypoint)
    :precondition 
      (and
        (at ?x0 ?x2)
        (at_rock_sample ?x2)
        (equipped_for_rock_analysis ?x0)
        (store_of ?x1 ?x0)
        (empty ?x1))
    :effect
      (and
        (full ?x1)
        (have_rock_analysis ?x0 ?x2)
        (not 
          (empty ?x1))
      )
    )
  (:action drop
    :parameters (?x0 - rover ?x1 - store)
    :precondition 
      (and
        (store_of ?x1 ?x0)
        (full ?x1))
    :effect
      (and
        (empty ?x1)
        (not 
          (full ?x1))
      )
    )
  (:action calibrate
    :parameters (?x0 - rover ?x1 - camera ?x2 - objective ?x3 - waypoint)
    :precondition 
      (and
        (equipped_for_imaging ?x0)
        (calibration_target ?x1 ?x2)
        (at ?x0 ?x3)
        (visible_from ?x2 ?x3)
        (on_board ?x1 ?x0))
    :effect
(calibrated ?x1 ?x0)    )
  (:action take_image
    :parameters (?x0 - rover ?x1 - waypoint ?x2 - objective ?x3 - camera ?x4 - mode)
    :precondition 
      (and
        (calibrated ?x3 ?x0)
        (on_board ?x3 ?x0)
        (equipped_for_imaging ?x0)
        (supports ?x3 ?x4)
        (visible_from ?x2 ?x1)
        (at ?x0 ?x1))
    :effect
      (and
        (oneof
          (have_image ?x0 ?x2 ?x4)
          (and)
        )
        (not 
          (calibrated ?x3 ?x0))
      )
    )
  (:action communicate_soil_data
    :parameters (?x0 - rover ?x1 - lander ?x2 - waypoint ?x3 - waypoint ?x4 - waypoint)
    :precondition 
      (and
        (at ?x0 ?x3)
        (at_lander ?x1 ?x4)
        (have_soil_analysis ?x0 ?x2)
        (visible ?x3 ?x4)
        (available ?x0)
        (channel_free ?x1))
    :effect
      (and
        (channel_free ?x1)
        (oneof
          (communicatedsoildata ?x2)
          (and)
        )
        (not
          (have_soil_analysis ?x0 ?x2))
        (available ?x0)
        (not 
          (channel_free ?x1))
      )
    )
  (:action communicate_rock_data
    :parameters (?x0 - rover ?x1 - lander ?x2 - waypoint ?x3 - waypoint ?x4 - waypoint)
    :precondition 
      (and
        (at ?x0 ?x3)
        (at_lander ?x1 ?x4)
        (have_rock_analysis ?x0 ?x2)
        (visible ?x3 ?x4)
        (available ?x0)
        (channel_free ?x1))
    :effect
      (and
        (channel_free ?x1)
        (oneof
          (communicatedrockdata ?x2)
          (and)
        )
        (not 
          (have_rock_analysis ?x0 ?x2))
        (available ?x0)
        (not 
          (channel_free ?x1))
      )
    )
  (:action communicate_image_data
    :parameters (?x0 - rover ?x1 - lander ?x2 - objective ?x3 - mode ?x4 - waypoint ?x5 - waypoint)
    :precondition 
      (and
        (at ?x0 ?x4)
        (at_lander ?x1 ?x5)
        (have_image ?x0 ?x2 ?x3)
        (visible ?x4 ?x5)
        (available ?x0)
        (channel_free ?x1))
    :effect
      (and
        (channel_free ?x1)
        (oneof 
          (communicatedimagedata ?x2 ?x3)
          (and)
        )
        (not
          (have_image ?x0 ?x2 ?x3))
        (available ?x0)
        (not 
          (channel_free ?x1))
      )
    )
)