module LDraw where

import Graphics.Gloss (Picture (Blank, Line, Pictures))
import LLang.Syntax (Action (..), Direction (Backward, Forward), Side (LeftSide, RightSide), symbols)

type Pos = (Float, Float)

type Stack = [(Pos, Float)]

type Status = (Pos, Stack, Float, Float)

drawLSystem :: Float -> String -> Picture
drawLSystem angle str = Pictures $ interpret ((0, 0), [], pi / 2, angle) str

interpret :: Status -> String -> [Picture]
interpret _ [] = [Blank]
interpret status (c : rest) = case lookup c symbols of
  Nothing -> Blank : interpret status rest
  Just action -> interpretAction status action rest

interpretAction :: Status -> Action -> String -> [Picture]
interpretAction (pos@(x, y), stack, angle, angleOffset) (Draw Forward) rest =
  let newPos = (x + cos angle, y + sin angle)
   in Line [pos, newPos] : interpret (newPos, stack, angle, angleOffset) rest
interpretAction (pos@(x, y), stack, angle, angleOffset) (Draw Backward) rest =
  let newPos = (x - cos angle, y - sin angle)
   in Line [pos, newPos] : interpret (newPos, stack, angle, angleOffset) rest
interpretAction (pos@(x, y), stack, angle, angleOffset) (Move Forward) rest =
  let newPos = (x + cos angle, y + sin angle)
   in interpret (newPos, stack, angle, angleOffset) rest
interpretAction (pos@(x, y), stack, angle, angleOffset) (Move Backward) rest =
  let newPos = (x - cos angle, y - sin angle)
   in interpret (newPos, stack, angle, angleOffset) rest
interpretAction (pos, stack, angle, angleOffset) (Rotation RightSide) rest = interpret (pos, stack, angle + angleOffset * pi / 180, angleOffset) rest
interpretAction (pos, stack, angle, angleOffset) (Rotation LeftSide) rest = interpret (pos, stack, angle - angleOffset * pi / 180, angleOffset) rest
interpretAction (pos, stack, angle, angleOffset) Push rest = interpret (pos, (pos, angle) : stack, angle, angleOffset) rest
interpretAction (_, (pos, angle) : stack, _, angleOffset) Pop rest = interpret (pos, stack, angle, angleOffset) rest
interpretAction (_, [], _, _) Pop _ = error "Tried to pop empty stack"
