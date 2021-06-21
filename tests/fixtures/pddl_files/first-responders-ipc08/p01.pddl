(define (problem FR_1_1)
 (:domain first-response)
 (:objects  l1  - location
	    f1 - fire_unit
	    v1 - victim
	    m1 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l1)
     (water-at l1)
	;;disaster info
     (fire l1)
     (victim-at v1 l1)
     (victim-status v1 hurt)
	;;map info
	(adjacent l1 l1)
	(fire-unit-at f1 l1)
	(medical-unit-at m1 l1)
	)
 (:goal (and  (nfire l1)  (victim-status v1 healthy)))
 )
