(define (problem truck-car-1)
  (:domain tire-truck)
  (:objects n0 ng - location
            n1 n2 n3 - location
            t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 - tire)
            
  (:init (initial-location n0)
  		 (car-at n0)
   		 (truck-at n0)
   		 (not-flattire)

       (car-spiky-road n0 n1) (car-spiky-road n1 n0)
       (car-spiky-road n1 n2) (car-spiky-road n2 n1)
       (car-road n2 n3) (car-road n3 n2)
       (car-road n3 ng) (car-road ng n3)

       (truck-road n0 n1) (truck-road n1 n0)
       (truck-road n1 n2) (truck-road n2 n1)
       (truck-road n2 n3) (truck-road n3 n2)
       (truck-road n3 ng) (truck-road ng n3)

       (free n0)
       (free n1)
       (free n2)
       (free n3)
       (free ng)

       (tire-at t1 n0)
       (tire-at t2 n0)
       (tire-at t3 n0)
       (tire-at t4 n0)
       (tire-at t5 n0)
       (tire-at t6 n0)
       (tire-at t7 n0)
       (tire-at t8 n0)
       ;(tire-at t9 n0)
       ;(tire-at t10 n0)
       ;(tire-at t11 n0)
       ;(tire-at t12 n0)

  )
(:goal (car-at ng)))