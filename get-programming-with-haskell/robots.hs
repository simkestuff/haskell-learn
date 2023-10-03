robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _)   = n 
attack (_, a, _) = a
hp (_, _, h)     = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- setName aRobot newName = robot (newName, attack', hp')
--     where attack' = getAtltack aRobot
--           hp' = getHP aRobot
--
-- setAttack aRobot newAttack = robot (name', newAttack, hp')
--     where name' = getName aRobot
--           hp' = getHP aRobot
--     
-- setHP aRobot newHP = robot (name', attack', newHP)
--     where attack' = getAttack aRobot
--           name' = getName aRobot 

setName aRobot newName = aRobot (\(_, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, _, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, _) -> robot (n, a, newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack: " ++ show a ++
                                        " hit points: " ++ show h)


-- ne znam zašto ali ovo ne prolazi
damage2 aRobot attackDamage = setHP aRobot h  
    where h = max (getHP aRobot - attackDamage) 0

damage rRobot attackDamage = rRobot (\(n,a,h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack 
    where attack
             | getHP aRobot > 0 = getAttack aRobot
             | otherwise          = 0 


-- fastRobot = robot ("speedy", 15, 40)
-- slowRobot = robot ("slowpoke",20,30)

-- fighting 3 rounds, each robot hit at the same time
-- fastRobotRound1 = fight slowRobot fastRobot
-- slowRobotRound1 = fight fastRobot slowRobot
-- fastRobotRound2 = fight slowRobotRound1 fastRobotRound1
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- fastRobotRound3 = fight slowRobotRound2 fastRobotRound2
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2

-- change priority, sequential hits
-- slowRobotRound1 = fight fastRobot slowRobot
-- fastRobotRound1 = fight slowRobotRound1 fastRobot
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
-- fastRobotRound3 = fight slowRobotRound3 fastRobotRound2

-- order has no importance
-- fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
-- fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
-- fastRobotRound1 = fight slowRobotRound1 fastRobot
-- slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
-- slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
-- slowRobotRound1 = fight fastRobot slowRobot

lifes lor = map getHP lor

threeRoundFight = 
    let    
           fastRobot = robot ("speedy", 15, 40)
           slowRobot = robot ("slowpoke",20,30)
           slowRobotRound1 = fight fastRobot slowRobot                 
           fastRobotRound1 = fight slowRobotRound1 fastRobot
           slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
           fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
           slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
           fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
    in lifes [fastRobotRound3, slowRobotRound3]
