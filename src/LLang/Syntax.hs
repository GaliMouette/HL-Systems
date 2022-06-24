module LLang.Syntax (LSystem (..), LHeader (..), LRule (..), Action (..), Direction (..), Side (..), symbols) where

data LSystem = LSystem {headers :: [LHeader], rules :: [LRule]}
  deriving (Show)

data LHeader = Axiom String | Angle Float
  deriving (Show)

data LRule = LRule {input :: Char, output :: String}
  deriving (Show)

data Action = Draw Direction | Move Direction | Rotation Side | Push | Pop
  deriving (Show)

data Direction = Forward | Backward
  deriving (Show)

data Side = LeftSide | RightSide
  deriving (Show)

symbols :: [(Char, Action)]
symbols =
  [ ('F', Draw Forward),
    ('B', Draw Backward),
    ('f', Move Forward),
    ('b', Move Backward),
    ('+', Rotation RightSide),
    ('-', Rotation LeftSide),
    ('[', Push),
    (']', Pop)
  ]
