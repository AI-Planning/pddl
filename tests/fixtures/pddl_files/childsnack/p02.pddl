;; base case
;;
(define (problem childsnack-02)
 (:domain childsnack)
 (:objects
    child1 - child
    tray1 - tray
    sandw1 - sandwich
    bread1 - bread-portion
    content1 - content-portion
    table1 - place
    )
 (:init
    (at tray1 kitchen)
    (at_kitchen_bread bread1)
    (at_kitchen_content content1)
    (no_gluten_bread bread1)
    (no_gluten_content content1)
    (allergic_gluten child1)
    (waiting child1 table1)
    (notexist sandw1)
 )
 (:goal  (and
    (served child1)
 )))
