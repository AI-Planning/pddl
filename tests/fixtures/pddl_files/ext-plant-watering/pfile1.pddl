(define (problem instance_10_5_2_1)
(:domain ext-plant-watering)
(:objects
	plant1 - plant
	plant2 - plant
	plant3 - plant
	plant4 - plant
	plant5 - plant
	tap1 - tap
	agent1 - agent
	agent2 - agent
)
(:init
	(= (maxx) 10)
	(= (minx) 1)
	(= (maxy) 10)
	(= (miny) 1)
	(= (total_poured) 0)
	(= (total_loaded) 0)
	(= (water_reserve) 29)
	(= (carrying agent1) 0)
	(= (max_carry agent1) 5)
	(= (carrying agent2) 0)
	(= (max_carry agent2) 5)
	(= (poured plant1) 0)
	(= (poured plant2) 0)
	(= (poured plant3) 0)
	(= (poured plant4) 0)
	(= (poured plant5) 0)
	(= (x plant1) 5)
	(= (y plant1) 7)
	(= (x plant2) 2)
	(= (y plant2) 7)
	(= (x plant3) 6)
	(= (y plant3) 2)
	(= (x plant4) 8)
	(= (y plant4) 7)
	(= (x plant5) 9)
	(= (y plant5) 1)
	(= (x tap1) 3)
	(= (y tap1) 4)
	(= (x agent1) 3)
	(= (y agent1) 7)
	(= (x agent2) 8)
	(= (y agent2) 6)
)
(:goal
(and
	(= (poured plant1) 4)
	(= (poured plant2) 2)
	(= (poured plant3) 7)
	(= (poured plant4) 9)
	(= (poured plant5) 5)
	(= (total_poured) (total_loaded))
)))
