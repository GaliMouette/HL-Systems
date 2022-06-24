module LSystem where

import Graphics.Gloss (Display (FullScreen), display, white)
import LDraw (drawLSystem)
import LLang.Parser (system)
import LLang.Syntax (LHeader (..), LRule (LRule), LSystem (LSystem))
import LOptions (LOptions (LOptions))
import Text.Megaparsec (errorBundlePretty, parse)

startLSystem :: LOptions -> IO ()
startLSystem options@(LOptions _ _ file) = do
  content <- readFile file
  case parse system file content of
    Left errors -> putStr (errorBundlePretty errors)
    Right system -> interpretLSystem options system

interpretLSystem :: LOptions -> LSystem -> IO ()
interpretLSystem options (LSystem headers rules) =
  let angle = findAngle headers; maybeAxiom = findAxiom headers
   in case maybeAxiom of
        Nothing -> putStrLn "Missing starting axiom"
        Just axiom -> animateLSystem options axiom angle rules

animateLSystem :: LOptions -> String -> Float -> [LRule] -> IO ()
animateLSystem (LOptions n anim _) axiom angle rules =
  display FullScreen white $ drawLSystem angle $ last $ iterateLSystem n axiom rules

iterateLSystem :: Int -> [Char] -> [LRule] -> [[Char]]
iterateLSystem n axiom rules =
  let iterations = iterate (transform rules) axiom
   in if n > 0 then take n iterations else iterations

transform :: [LRule] -> [Char] -> String
transform rules = concatMap (findRule rules)

findAxiom :: [LHeader] -> Maybe String
findAxiom [] = Nothing
findAxiom (Axiom axiom : rest) = Just axiom
findAxiom (_ : rest) = findAxiom rest

findAngle :: [LHeader] -> Float
findAngle [] = 0
findAngle (Angle angle : rest) = angle
findAngle (_ : rest) = findAngle rest

findRule :: [LRule] -> Char -> String
findRule [] c = [c]
findRule ((LRule input output) : rest) c =
  if c == input then output else findRule rest c
