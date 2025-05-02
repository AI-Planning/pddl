(define (problem instance_12_15_2_1)
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
	plant12 - plant
	plant13 - plant
	plant14 - plant
	plant15 - plant
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
	(= (water_reserve) 83)
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
	(= (poured plant12) 0)
	(= (poured plant13) 0)
	(= (poured plant14) 0)
	(= (poured plant15) 0)
	(= (x plant1) 10)
	(= (y plant1) 8)
	(= (x plant2) 6)
	(= (y plant2) 2)
	(= (x plant3) 2)
	(= (y plant3) 6)
	(= (x plant4) 3)
	(= (y plant4) 5)
	(= (x plant5) 8)
	(= (y plant5) 3)
	(= (x plant6) 1)
	(= (y plant6) 1)
	(= (x plant7) 5)
	(= (y plant7) 8)
	(= (x plant8) 4)
	(= (y plant8) 1)
	(= (x plant9) 12)
	(= (y plant9) 4)
	(= (x plant10) 12)
	(= (y plant10) 7)
	(= (x plant11) 8)
	(= (y plant11) 6)
	(= (x plant12) 1)
	(= (y plant12) 10)
	(= (x plant13) 8)
	(= (y plant13) 12)
	(= (x plant14) 1)
	(= (y plant14) 12)
	(= (x plant15) 11)
	(= (y plant15) 1)
	(= (x tap1) 7)
	(= (y tap1) 8)
	(= (x agent1) 3)
	(= (y agent1) 4)
	(= (x agent2) 7)
	(= (y agent2) 7)
)
(:goal
(and
	(= (poured plant1) 4)
	(= (poured plant2) 3)
	(= (poured plant3) 6)
	(= (poured plant4) 1)
	(= (poured plant5) 10)
	(= (poured plant6) 10)
	(= (poured plant7) 1)
	(= (poured plant8) 10)
	(= (poured plant9) 10)
	(= (poured plant10) 7)
	(= (poured plant11) 2)
	(= (poured plant12) 8)
	(= (poured plant13) 1)
	(= (poured plant14) 2)
	(= (poured plant15) 1)
	(= (total_poured) (total_loaded))
)))
