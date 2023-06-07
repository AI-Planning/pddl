(define (problem sussman-anomaly)       ; graphplan 3 steps
    (:domain blocks-world-domain)
  (:objects A B C)
  (:init (block A) (block B) (block C) (block Table)
	 (on C A) (on A Table) (on B Table)
	 (clear C) (clear B) (clear Table))
  (:goal (on B A))
)
