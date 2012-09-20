module QuickCheckTest where
import Test.QuickCheck

-- World
-- oppositedir
-- turnleft
-- turnright
-- move
-- dist
-- withinBounds
-- fromList
-- validMove
-- 
-- MEL
-- interp
-- Forward
-- forward i hver retning, med vaeg og uden vaeg.
-- forskellige positioner
-- Backward 
-- Samme som for forward
-- TurnLeft
-- Maa ikke vaere identitet
-- TurnLeft x 4 = start direction
-- TurnRight
-- samme som for TurnLeft
-- If
-- teste hvad der sker hvis sand, teste hvad der sker hvis falsk
-- While
-- Teste udtryk vi ved der bliver falske
-- Block
-- Lave en raekke statements og se om vi faar det forventede resultat
-- evalC

-- Wall
-- hver retning med vaeg
-- hver retning uden vaeg
-- proever i forskellige position

-- And
-- sand ,sand
-- sand fals
-- falsk sand
-- falsk falsk

-- Not
-- sand -> falsk
-- falsk -> sand
--
-- AtGoalPos
-- teste i goalpos med forskellige bounds
-- teste ikke i goalpos med de samme bounds
-- 
