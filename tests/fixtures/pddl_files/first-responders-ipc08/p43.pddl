(define (problem FR_4_3)
 (:domain first-response)
 (:objects  l1 l2 l3 l4  - location
	    f1 - fire_unit
	    v1 v2 v3 - victim
	    m1 m2 m3 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l4)
     (hospital l1)
     (water-at l4)
	;;disaster info
     (fire l4)
     (victim-at v1 l1)
     (victim-status v1 dying)
     (fire l4)
     (victim-at v2 l4)
     (victim-status v2 dying)
     (fire l3)
     (victim-at v3 l4)
     (victim-status v3 hurt)
	;;map info
	(adjacent l1 l1)
	(adjacent l2 l2)
	(adjacent l3 l3)
	(adjacent l4 l4)
   (adjacent l1 l1)
   (adjacent l1 l1)
   (adjacent l1 l2)
   (adjacent l2 l1)
   (adjacent l3 l1)
   (adjacent l1 l3)
   (adjacent l3 l2)
   (adjacent l2 l3)
   (adjacent l4 l1)
   (adjacent l1 l4)
	(fire-unit-at f1 l4)
	(medical-unit-at m1 l1)
	(medical-unit-at m2 l1)
	(medical-unit-at m3 l2)
	)
 (:goal (and  (nfire l4) (nfire l4) (nfire l3)  (victim-status v1 healthy) (victim-status v2 healthy) (victim-status v3 healthy)))
 )
