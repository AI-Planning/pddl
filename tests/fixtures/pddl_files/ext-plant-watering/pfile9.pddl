(define (problem instance_14_13_2_1)
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
	(= (maxx) 14)
	(= (minx) 1)
	(= (maxy) 14)
	(= (miny) 1)
	(= (total_poured) 0)
	(= (total_loaded) 0)
	(= (water_reserve) 92)
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
	(= (x plant1) 14)
	(= (y plant1) 1)
	(= (x plant2) 13)
	(= (y plant2) 11)
	(= (x plant3) 8)
	(= (y plant3) 10)
	(= (x plant4) 11)
	(= (y plant4) 5)
	(= (x plant5) 2)
	(= (y plant5) 6)
	(= (x plant6) 14)
	(= (y plant6) 6)
	(= (x plant7) 9)
	(= (y plant7) 12)
	(= (x plant8) 12)
	(= (y plant8) 12)
	(= (x plant9) 1)
	(= (y plant9) 6)
	(= (x plant10) 11)
	(= (y plant10) 12)
	(= (x plant11) 2)
	(= (y plant11) 5)
	(= (x plant12) 2)
	(= (y plant12) 14)
	(= (x plant13) 1)
	(= (y plant13) 9)
	(= (x tap1) 6)
	(= (y tap1) 5)
	(= (x agent1) 3)
	(= (y agent1) 6)
	(= (x agent2) 1)
	(= (y agent2) 7)
)
(:goal
(and
	(= (poured plant1) 5)
	(= (poured plant2) 9)
	(= (poured plant3) 7)
	(= (poured plant4) 2)
	(= (poured plant5) 1)
	(= (poured plant6) 10)
	(= (poured plant7) 9)
	(= (poured plant8) 10)
	(= (poured plant9) 6)
	(= (poured plant10) 6)
	(= (poured plant11) 7)
	(= (poured plant12) 5)
	(= (poured plant13) 7)
	(= (total_poured) (total_loaded))
)))
