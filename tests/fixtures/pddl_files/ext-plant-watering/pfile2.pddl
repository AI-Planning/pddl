(define (problem instance_11_5_2_1)
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
	(= (maxx) 11)
	(= (minx) 1)
	(= (maxy) 11)
	(= (miny) 1)
	(= (total_poured) 0)
	(= (total_loaded) 0)
	(= (water_reserve) 30)
	(= (carrying agent1) 0)
	(= (max_carry agent1) 5)
	(= (carrying agent2) 0)
	(= (max_carry agent2) 5)
	(= (poured plant1) 0)
	(= (poured plant2) 0)
	(= (poured plant3) 0)
	(= (poured plant4) 0)
	(= (poured plant5) 0)
	(= (x plant1) 2)
	(= (y plant1) 9)
	(= (x plant2) 10)
	(= (y plant2) 2)
	(= (x plant3) 4)
	(= (y plant3) 8)
	(= (x plant4) 8)
	(= (y plant4) 2)
	(= (x plant5) 8)
	(= (y plant5) 8)
	(= (x tap1) 9)
	(= (y tap1) 1)
	(= (x agent1) 10)
	(= (y agent1) 8)
	(= (x agent2) 4)
	(= (y agent2) 2)
)
(:goal
(and
	(= (poured plant1) 10)
	(= (poured plant2) 1)
	(= (poured plant3) 8)
	(= (poured plant4) 8)
	(= (poured plant5) 1)
	(= (total_poured) (total_loaded))
)))
