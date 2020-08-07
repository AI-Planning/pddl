(define (problem FR_4_1)
 (:domain first-response)
 (:objects  l1 l2 l3 l4  - location
	    f1 - fire_unit
	    v1 - victim
	    m1 m2 m3 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l4)
     (water-at l3)
     (water-at l3)
     (water-at l4)
     (water-at l4)
	;;disaster info
     (fire l1)
     (victim-at v1 l2)
     (victim-status v1 hurt)
	;;map info
	(adjacent l1 l1)
	(adjacent l2 l2)
	(adjacent l3 l3)
	(adjacent l4 l4)
   (adjacent l2 l1)
   (adjacent l1 l2)
   (adjacent l2 l2)
   (adjacent l2 l2)
   (adjacent l4 l1)
   (adjacent l1 l4)
   (adjacent l4 l2)
   (adjacent l2 l4)
	(fire-unit-at f1 l2)
	(medical-unit-at m1 l2)
	(medical-unit-at m2 l3)
	(medical-unit-at m3 l4)
	)
 (:goal (and  (nfire l1)  (victim-status v1 healthy)))
 )
