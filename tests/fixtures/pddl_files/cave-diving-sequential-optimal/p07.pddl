;; Cave Diving ADL
;; Authors: Nathan Robinson,
;;          Christian Muise, and
;;          Charles Gretton

(define (problem cave-diving-adl-p02)
  (:domain cave-diving-adl)
  (:objects
    l0 l1 l2 - location
    d0 d1 - diver
    t0 t1 t2 t3 t4 t5 t6 dummy - tank
    zero one two three four - quantity
  )

  (:init
    (available d0)
    (available d1)
    (capacity d0 four)
    (capacity d1 four)
    (in-storage t0)
    (next-tank t0 t1)
    (next-tank t1 t2)
    (next-tank t2 t3)
    (next-tank t3 t4)
    (next-tank t4 t5)
    (next-tank t5 t6)
    (next-tank t6 dummy)
    (cave-entrance l0)
    (connected l0 l1)
    (connected l1 l0)
    (connected l1 l2)
    (connected l2 l1)
    (next-quantity zero one)
    (next-quantity one two)
    (next-quantity two three)
    (next-quantity three four)
    (precludes d0 d1)
    (= (hiring-cost d0) 11)
    (= (hiring-cost d1) 63)
    (= (other-cost ) 1)
    (= (total-cost) 0)
  )

  (:goal
    (and
      (have-photo l2)
      (decompressing d0)
      (decompressing d1)
    )
  )

  (:metric minimize (total-cost))

)
