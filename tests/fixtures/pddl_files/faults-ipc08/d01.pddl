(define (domain faults)
 (:types operation fault)
 (:constants  f1 - fault 
              o1 - operation)
 (:predicates 
   (not_completed ?o - operation)
   (completed ?o - operation)
   (fault ?f - fault)
   (not_fault ?f - fault)
   (faulted_op ?o - operation ?f - fault)
   (last_fault ?f - fault)
   (made)
  )

 (:action perform_operation_1_fault
  :parameters (?o - operation)
  :precondition (and  (not_fault f1) (not_completed ?o))
  :effect (and (completed ?o) (not (not_completed ?o))
               (oneof (and) (and (fault f1) (not (not_fault f1))
                                 (faulted_op ?o f1) (last_fault f1))))
 )
 (:action repair_fault_1
  :parameters (?o - operation)
  :precondition (and (faulted_op ?o f1) (last_fault f1))
  :effect (and (not (faulted_op ?o f1))
               (not_completed ?o) (not (completed ?o))
               (not (last_fault f1)) (not_fault f1)
          )
  )
 (:action finish 
  :precondition (and  (completed o1) (not (last_fault f1)))
  :effect (made)
 )
)
