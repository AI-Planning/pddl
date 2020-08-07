(define (problem FR_3_3)
 (:domain first-response)
 (:objects  l1 l2 l3  - location
	    f1 - fire_unit
	    v1 v2 v3 - victim
	    m1 - medical_unit
)
 (:init 
	;;strategic locations
     (hospital l1)
     (water-at l1)
	;;disaster info
     (fire l1)
     (victim-at v1 l3)
     (victim-status v1 hurt)
     (fire l2)
     (victim-at v2 l2)
     (victim-status v2 dying)
     (fire l2)
     (victim-at v3 l1)
     (victim-status v3 dying)
	;;map info
	(adjacent l1 l1)
	(adjacent l2 l2)
	(adjacent l3 l3)
   (adjacent l1 l1)
   (adjacent l1 l1)
   (adjacent l1 l2)
   (adjacent l2 l1)
   (adjacent l2 l1)
   (adjacent l1 l2)
   (adjacent l2 l2)
   (adjacent l2 l2)
   (adjacent l3 l1)
   (adjacent l1 l3)
   (adjacent l3 l2)
   (adjacent l2 l3)
	(fire-unit-at f1 l3)
	(medical-unit-at m1 l3)
	)
 (:goal (and  (nfire l1) (nfire l2) (nfire l2)  (victim-status v1 healthy) (victim-status v2 healthy) (victim-status v3 healthy)))
 )
