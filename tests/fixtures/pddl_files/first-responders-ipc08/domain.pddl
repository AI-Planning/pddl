(define (domain first-response)
  (:requirements :typing
		 :equality
		 :negative-preconditions
		 :disjunctive-preconditions
		 :universal-preconditions
		 :conditional-effects
		 :existential-preconditions
		 :non-deterministic)
 (:types
   location victim status fire_unit medical_unit - object
 )
 (:constants healthy hurt dying - status)
 (:predicates
  (fire ?l - location)
  (nfire ?l - location)
  (victim-at ?v - victim ?l - location)
  (victim-status ?v - victim ?s - status)
  (hospital ?l - location)
  (water-at ?l - location)
  (adjacent ?l1 ?l2 - location)
  (fire-unit-at ?u - fire_unit ?l - location)
  (medical-unit-at ?u - medical_unit ?l - location)
  (have-water ?u - fire_unit)
  (have-victim-in-unit ?v - victim ?u - medical_unit)
  )

 (:action drive-fire-unit
  :parameters (?u - fire_unit ?from - location ?to - location)
  :precondition (and (fire-unit-at ?u ?from)
		     (adjacent ?to ?from)
		     (not (fire ?to))
		     )
  :effect (and (fire-unit-at ?u ?to) (not (fire-unit-at ?u ?from)))
  )

 (:action drive-medical-unit
  :parameters (?u - medical_unit ?from - location ?to - location)
  :precondition (and (medical-unit-at ?u ?from)
		     (adjacent ?to ?from)
		     (not (fire ?to))
		     )
  :effect (and (medical-unit-at ?u ?to) (not (medical-unit-at ?u ?from)))
  )



 (:action load-fire-unit
  :parameters (?u - fire_unit ?l - location)
  :precondition (and (fire-unit-at ?u ?l) (water-at ?l))
  :effect (have-water ?u))

 (:action load-medical-unit
  :parameters (?u - medical_unit ?l - location ?v - victim)
  :precondition (and (medical-unit-at ?u ?l) (victim-at ?v ?l))
  :effect (and (have-victim-in-unit ?v ?u)
	       (not (victim-at ?v ?l))))



 (:action unload-fire-unit
  :parameters (?u - fire_unit ?l ?l1 - location)
  :precondition (and (fire-unit-at ?u ?l)
                     (adjacent ?l1 ?l)
                     (have-water ?u)
                     (fire ?l1))
  :effect (and (not (have-water ?u))
 	       (oneof (and) (and (nfire ?l1) (not (fire ?l1))))))

 (:action unload-medical-unit
  :parameters (?u - medical_unit ?l - location ?v - victim)
  :precondition (and (medical-unit-at ?u ?l)(have-victim-in-unit ?v ?u))
  :effect (and (victim-at ?v ?l) (not (have-victim-in-unit ?v ?u))))


 (:action treat-victim-on-scene-medical
  :parameters (?u - medical_unit ?l - location ?v - victim)
  :precondition (and (medical-unit-at ?u ?l)
		     (victim-at ?v ?l)
		     (victim-status ?v hurt))
  :effect (oneof (and) (and (victim-status ?v healthy)
			    (not (victim-status ?v hurt)))))

 (:action treat-victim-on-scene-fire
  :parameters (?u - fire_unit ?l - location ?v - victim)
  :precondition (and (fire-unit-at ?u ?l)
		     (victim-at ?v ?l)
		     (victim-status ?v hurt))
  :effect (oneof (and) (and (victim-status ?v healthy)
			    (not (victim-status ?v hurt)))))

 (:action treat-victim-at-hospital
  :parameters (?v - victim ?l - location)
  :precondition (and (victim-at ?v ?l)
		     (hospital ?l))
  :effect (and (victim-status ?v healthy)
	       (not (victim-status ?v hurt))
	       (not (victim-status ?v dying))))

)
