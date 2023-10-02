robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _)   = n 
attack (_, a, _) = a
hp (_, _, h)     = h

getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- setName aRobot newName = robot (newName, attack', hp')
--     where attack' = getAttack aRobot
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
