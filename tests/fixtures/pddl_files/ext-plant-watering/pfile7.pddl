(define (problem instance_10_11_2_1)
(:domain ext-plant-watering)
(:objects
	plant1 - plant
	plant2 - plant
	plant3 - plant
	plant4 - plant
	plant5 - plant
	plant6 - plant
	plant7 - plant
	plant8 - plant
	plant9 - plant
	plant10 - plant
	plant11 - plant
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
	(= (water_reserve) 71)
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
	(= (poured plant8) 0)
	(= (poured plant9) 0)
	(= (poured plant10) 0)
	(= (poured plant11) 0)
	(= (x plant1) 1)
	(= (y plant1) 8)
	(= (x plant2) 1)
	(= (y plant2) 1)
	(= (x plant3) 10)
	(= (y plant3) 8)
	(= (x plant4) 3)
	(= (y plant4) 10)
	(= (x plant5) 5)
	(= (y plant5) 4)
	(= (x plant6) 2)
	(= (y plant6) 7)
	(= (x plant7) 1)
	(= (y plant7) 3)
	(= (x plant8) 10)
	(= (y plant8) 7)
	(= (x plant9) 5)
	(= (y plant9) 5)
	(= (x plant10) 7)
	(= (y plant10) 6)
	(= (x plant11) 10)
	(= (y plant11) 2)
	(= (x tap1) 8)
	(= (y tap1) 5)
	(= (x agent1) 7)
	(= (y agent1) 10)
	(= (x agent2) 3)
	(= (y agent2) 1)
)
(:goal
(and
	(= (poured plant1) 5)
	(= (poured plant2) 1)
	(= (poured plant3) 6)
	(= (poured plant4) 5)
	(= (poured plant5) 9)
	(= (poured plant6) 4)
	(= (poured plant7) 10)
	(= (poured plant8) 7)
	(= (poured plant9) 5)
	(= (poured plant10) 6)
	(= (poured plant11) 7)
	(= (total_poured) (total_loaded))
)))
