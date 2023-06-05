; Map of the Depots:      
; 0*0 
;---- 
; 0: depot0 area
; *: Depot access point
; =: Transit area

(define (problem storage-3)
(:domain Storage-Propositional)
(:objects
	depot0-1-1 depot0-1-2 depot0-1-3 container-0-0 - storearea
	hoist0 hoist1 hoist2 - hoist
	crate0 - crate
	container0 - container
	depot0 - depot
	loadarea - transitarea)

(:init
	(connected depot0-1-1 depot0-1-2)
	(connected depot0-1-2 depot0-1-3)
	(connected depot0-1-2 depot0-1-1)
	(connected depot0-1-3 depot0-1-2)
	(in depot0-1-1 depot0)
	(in depot0-1-2 depot0)
	(in depot0-1-3 depot0)
	(on crate0 container-0-0)
	(in crate0 container0)
	(in container-0-0 container0)
	(connected loadarea container-0-0) 
	(connected container-0-0 loadarea)  
	(connected depot0-1-2 loadarea)
	(connected loadarea depot0-1-2)    
	(at hoist0 depot0-1-2)
	(available hoist0)
	(at hoist1 depot0-1-3)
	(available hoist1)
	(at hoist2 depot0-1-1)
	(available hoist2))

(:goal (and
	(in crate0 depot0)))
)
