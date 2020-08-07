(define (problem FR_2_3)
 (:domain first-response)
 (:objects  l1 l2  - location
	    f1 - fire_unit
	    v1 v2 v3 - victim
	    m1 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l1)
     (water-at l2)
     (water-at l1)
	;;disaster info
     (fire l2)
     (victim-at v1 l2)
     (victim-status v1 dying)
     (fire l1)
     (victim-at v2 l2)
     (victim-status v2 hurt)
     (fire l1)
     (victim-at v3 l1)
     (victim-status v3 hurt)
	;;map info
	(adjacent l1 l1)
	(adjacent l2 l2)
   (adjacent l1 l1)
   (adjacent l1 l1)
   (adjacent l2 l1)
   (adjacent l1 l2)
	(fire-unit-at f1 l1)
	(medical-unit-at m1 l1)
	)
 (:goal (and  (nfire l2) (nfire l1) (nfire l1)  (victim-status v1 healthy) (victim-status v2 healthy) (victim-status v3 healthy)))
 )
