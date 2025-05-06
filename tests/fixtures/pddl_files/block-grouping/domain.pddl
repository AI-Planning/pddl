;; Enrico Scala (enricos83@gmail.com) and Miquel Ramirez (miquel.ramirez@gmail.com)
;; Reference paper: Scala, Enrico, Patrik Haslum, Sylvie Thiébaux, and Miquel Ramirez. 
;;                  "Subgoaling techniques for satisficing and optimal numeric planning." 
;;                  Journal of Artificial Intelligence Research 68 (2020): 691-752.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Block grouping domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A number of blocks of different colours lie on a grid-like environment.
;;; The objective is to group the blocks by colour, i.e. to have all blocks
;;; of the same color in the same cell, which is at the same time
;;; different to the cell where blocks of other colors are:
;;;
;;; forall i, j color(i) = color(j) <=> loc(i) = loc(j)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (domain mt-block-grouping)
    (:requirements :strips :typing :numeric-fluents :disjunctive-preconditions)
    (:types
        block - object
    )
    (:functions
        (x ?b - block) ;; The position of a block
        (y ?b - block) ;;
        (max_x)
        (min_x)
        (max_y)
        (min_y)
    )

    ;; Move a block from its location to an adjacent location
    (:action move_block_up
        :parameters (?b - block)
        :precondition (and (<= (+ (y ?b) 1) (max_y)))
        :effect (and
            (increase (y ?b) 1)
        )
    )

    (:action move_block_down
        :parameters (?b - block)
        :precondition (and (>= (y ?b) (+ (min_y) 1)))
        :effect (and
            (decrease (y ?b) 1)
        )
    )

    (:action move_block_right
        :parameters (?b - block)
        :precondition (and (<= (+ (x ?b) 1) (max_x)))
        :effect (and
            (increase (x ?b) 1)
        )
    )

    (:action move_block_left
        :parameters (?b - block)
        :precondition (and (>= (x ?b) (+ (min_x) 1)))
        :effect (and
            (decrease (x ?b) 1)
        )
    )

)