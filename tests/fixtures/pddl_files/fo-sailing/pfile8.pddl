;; Enrico Scala (enricos83@gmail.com) and Miquel Ramirez (miquel.ramirez@gmail.com)
;;Setting seed to 1229
(define (problem instance_2_3_1229)

(:domain sailing_ln)

	(:objects
		b0 b1  - boat
		p0 p1 p2  - person
	)

  (:init
		(= (x b0) -7)
(= (y b0) 0)
(= (v b0) 1)

(= (x b1) -2)
(= (y b1) 0)
(= (v b1) 1)



		(= (d p0) -370)
(= (d p1) -58)
(= (d p2) 63)


	)

	(:goal
		(and
			(saved p0)
(saved p1)
(saved p2)

		)
	)
)


