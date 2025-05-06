;; Enrico Scala (enricos83@gmail.com) and Miquel Ramirez (miquel.ramirez@gmail.com)
;;Setting seed to 1229
(define (problem instance_1_4_1229)

(:domain sailing_ln)

	(:objects
		b0  - boat
		p0 p1 p2 p3  - person
	)

  (:init
		(= (x b0) -7)
(= (y b0) 0)
(= (v b0) 1)



		(= (d p0) -370)
(= (d p1) -58)
(= (d p2) 63)
(= (d p3) 483)


	)

	(:goal
		(and
			(saved p0)
(saved p1)
(saved p2)
(saved p3)

		)
	)
)


