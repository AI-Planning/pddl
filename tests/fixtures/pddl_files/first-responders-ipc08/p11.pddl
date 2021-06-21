(define (problem FR_2_1)
 (:domain first-response)
 (:objects  l1 l2  - location
	    f1 f2 - fire_unit
	    v1 - victim
	    m1 m2 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l1)
     (hospital l1)
     (water-at l2)
     (water-at l1)
	;;disaster info
     (fire l1)
     (victim-at v1 l2)
     (victim-status v1 dying)
	;;map info
	(adjacent l1 l1)
	(adjacent l2 l2)
	(fire-unit-at f1 l2)
	(fire-unit-at f2 l2)
	(medical-unit-at m1 l1)
	(medical-unit-at m2 l1)
	)
 (:goal (and  (nfire l1)  (victim-status v1 healthy)))
 )
