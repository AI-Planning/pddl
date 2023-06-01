; Map of the Depots:         
; 000    
; 0*0    
;----    
; 0: depot0 area
; *: Depot access point
; =: Transit area

(define (problem storage-9)
(:domain Storage-Propositional)
(:objects
	depot0-1-1 depot0-1-2 depot0-1-3 depot0-2-1 depot0-2-2 depot0-2-3 container-0-0 container-0-1 container-0-2 - storearea
	hoist0 hoist1 hoist2 - hoist
	crate0 crate1 crate2 - crate
	container0 - container
	depot0 - depot
	loadarea - transitarea)

(:init
	(connected depot0-1-1 depot0-2-1)
	(connected depot0-1-1 depot0-1-2)
	(connected depot0-1-2 depot0-2-2)
	(connected depot0-1-2 depot0-1-3)
	(connected depot0-1-2 depot0-1-1)
	(connected depot0-1-3 depot0-2-3)
	(connected depot0-1-3 depot0-1-2)
	(connected depot0-2-1 depot0-1-1)
	(connected depot0-2-1 depot0-2-2)
	(connected depot0-2-2 depot0-1-2)
	(connected depot0-2-2 depot0-2-3)
	(connected depot0-2-2 depot0-2-1)
	(connected depot0-2-3 depot0-1-3)
	(connected depot0-2-3 depot0-2-2)
	(in depot0-1-1 depot0)
	(in depot0-1-2 depot0)
	(in depot0-1-3 depot0)
	(in depot0-2-1 depot0)
	(in depot0-2-2 depot0)
	(in depot0-2-3 depot0)
	(on crate0 container-0-0)
	(on crate1 container-0-1)
	(on crate2 container-0-2)
	(in crate0 container0)
	(in crate1 container0)
	(in crate2 container0)
	(in container-0-0 container0)
	(in container-0-1 container0)
	(in container-0-2 container0)
	(connected loadarea container-0-0) 
	(connected container-0-0 loadarea)
	(connected loadarea container-0-1) 
	(connected container-0-1 loadarea)
	(connected loadarea container-0-2) 
	(connected container-0-2 loadarea)  
	(connected depot0-2-2 loadarea)
	(connected loadarea depot0-2-2)  
	(clear depot0-2-1)
	(clear depot0-1-2)
	(clear depot0-1-3)  
	(at hoist0 depot0-2-2)
	(available hoist0)
	(at hoist1 depot0-1-1)
	(available hoist1)
	(at hoist2 depot0-2-3)
	(available hoist2))

(:goal (and
	(in crate0 depot0)
	(in crate1 depot0)
	(in crate2 depot0)))
)
