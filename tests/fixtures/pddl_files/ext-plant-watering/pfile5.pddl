(define (problem instance_12_7_2_1)
(:domain ext-plant-watering)
(:objects
	plant1 - plant
	plant2 - plant
	plant3 - plant
	plant4 - plant
	plant5 - plant
	plant6 - plant
	plant7 - plant
	tap1 - tap
	agent1 - agent
	agent2 - agent
)
(:init
	(= (maxx) 12)
	(= (minx) 1)
	(= (maxy) 12)
	(= (miny) 1)
	(= (total_poured) 0)
	(= (total_loaded) 0)
	(= (water_reserve) 40)
	(= (carrying agent1) 0)
	(= (max_carry agent1) 5)
	(= (carrying agent2) 0)
	(= (max_carry agent2) 5)
	(= (poured plant1) 0)
	(= (poured plant2) 0)
	(= (poured plant3) 0)
	(= (poured plant4) 0)
	(= (poured plant5) 0)
	(= (poured plant6) 0)
	(= (poured plant7) 0)
	(= (x plant1) 9)
	(= (y plant1) 10)
	(= (x plant2) 10)
	(= (y plant2) 9)
	(= (x plant3) 7)
	(= (y plant3) 6)
	(= (x plant4) 1)
	(= (y plant4) 6)
	(= (x plant5) 3)
	(= (y plant5) 1)
	(= (x plant6) 12)
	(= (y plant6) 12)
	(= (x plant7) 3)
	(= (y plant7) 5)
	(= (x tap1) 12)
	(= (y tap1) 10)
	(= (x agent1) 2)
	(= (y agent1) 10)
	(= (x agent2) 2)
	(= (y agent2) 1)
)
(:goal
(and
	(= (poured plant1) 4)
	(= (poured plant2) 5)
	(= (poured plant3) 3)
	(= (poured plant4) 9)
	(= (poured plant5) 4)
	(= (poured plant6) 2)
	(= (poured plant7) 10)
	(= (total_poured) (total_loaded))
)))
