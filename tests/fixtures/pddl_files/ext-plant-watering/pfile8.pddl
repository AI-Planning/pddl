(define (problem instance_13_13_2_1)
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
	tap1 - tap
	agent1 - agent
	agent2 - agent
)
(:init
	(= (maxx) 13)
	(= (minx) 1)
	(= (maxy) 13)
	(= (miny) 1)
	(= (total_poured) 0)
	(= (total_loaded) 0)
	(= (water_reserve) 74)
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
	(= (x plant1) 1)
	(= (y plant1) 7)
	(= (x plant2) 3)
	(= (y plant2) 5)
	(= (x plant3) 4)
	(= (y plant3) 5)
	(= (x plant4) 11)
	(= (y plant4) 11)
	(= (x plant5) 2)
	(= (y plant5) 5)
	(= (x plant6) 5)
	(= (y plant6) 9)
	(= (x plant7) 10)
	(= (y plant7) 13)
	(= (x plant8) 11)
	(= (y plant8) 1)
	(= (x plant9) 11)
	(= (y plant9) 7)
	(= (x plant10) 13)
	(= (y plant10) 6)
	(= (x plant11) 1)
	(= (y plant11) 9)
	(= (x plant12) 7)
	(= (y plant12) 8)
	(= (x plant13) 11)
	(= (y plant13) 8)
	(= (x tap1) 12)
	(= (y tap1) 10)
	(= (x agent1) 9)
	(= (y agent1) 5)
	(= (x agent2) 6)
	(= (y agent2) 8)
)
(:goal
(and
	(= (poured plant1) 9)
	(= (poured plant2) 1)
	(= (poured plant3) 7)
	(= (poured plant4) 4)
	(= (poured plant5) 8)
	(= (poured plant6) 2)
	(= (poured plant7) 7)
	(= (poured plant8) 1)
	(= (poured plant9) 3)
	(= (poured plant10) 1)
	(= (poured plant11) 9)
	(= (poured plant12) 8)
	(= (poured plant13) 8)
	(= (total_poured) (total_loaded))
)))
