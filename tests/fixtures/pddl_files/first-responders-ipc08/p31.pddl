(define (problem FR_3_1)
 (:domain first-response)
 (:objects  l1 l2 l3  - location
	    f1 f2 - fire_unit
	    v1 - victim
	    m1 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l1)
     (hospital l1)
     (water-at l3)
     (water-at l3)
     (water-at l3)
	;;disaster info
     (fire l2)
     (victim-at v1 l2)
     (victim-status v1 hurt)
	;;map info
	(adjacent l1 l1)
	(adjacent l2 l2)
	(adjacent l3 l3)
   (adjacent l3 l1)
   (adjacent l1 l3)
   (adjacent l3 l2)
   (adjacent l2 l3)
	(fire-unit-at f1 l1)
	(fire-unit-at f2 l3)
	(medical-unit-at m1 l3)
	)
 (:goal (and  (nfire l2)  (victim-status v1 healthy)))
 )
