; There are mechanics who on any day may work at one
; of several cities where the airplane maintenance
; company has facilities. There are airplanes each of
; which has to be maintained during the given time period.
; The airplanes are guaranteed to visit some of the cities
; on given days. The problem is to schedule the presence
; of the mechanics so that each plane will get maintenance.

(define (domain maintenance-scheduling-domain)
 (:requirements :adl :typing :conditional-effects)
 (:types plane day airport)
 (:predicates  (done ?p - plane)
  (today ?d - day)
  (at ?p - plane ?d - day ?c - airport)
  (next ?d - day ?d2 - day) )

 (:action workat
  :parameters (?day - day ?airport - airport)
  :precondition (today ?day)
  :effect (and
     (not (today ?day))
     (forall (?plane - plane) (when (at ?plane ?day ?airport) (done ?plane)))))

)