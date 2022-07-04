module LSystem where

import Graphics.Gloss (Display (FullScreen), animate, display, white)
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
    Right lsystem -> interpretLSystem options lsystem

interpretLSystem :: LOptions -> LSystem -> IO ()
interpretLSystem options (LSystem headers rules) =
  let angle = findAngle headers
      maybeAxiom = findAxiom headers
   in case maybeAxiom of
        Nothing -> putStrLn "Missing starting axiom"
        Just axiom -> animateLSystem options axiom angle rules

animateLSystem :: LOptions -> String -> Float -> [LRule] -> IO ()
animateLSystem (LOptions n anim _) axiom angle rules =
  let iterations = iterateLSystem n axiom rules
      images = map (drawLSystem angle) iterations
   in if anim
        then animate FullScreen white $ \f -> images !! (floor (2 * f) `mod` length images)
        else display FullScreen white $ drawLSystem angle $ last iterations

iterateLSystem :: Int -> [Char] -> [LRule] -> [[Char]]
iterateLSystem n axiom rules =
  let iterations = iterate (transform rules) axiom
   in if n > 0
        then take n iterations
        else iterations

transform :: [LRule] -> [Char] -> String
transform rules = concatMap (findRule rules)

findAxiom :: [LHeader] -> Maybe String
findAxiom [] = Nothing
findAxiom (Axiom axiom : _) = Just axiom
findAxiom (_ : rest) = findAxiom rest

findAngle :: [LHeader] -> Float
findAngle [] = 0
findAngle (Angle angle : _) = angle * pi / 180
findAngle (_ : rest) = findAngle rest

findRule :: [LRule] -> Char -> String
findRule [] c = [c]
findRule ((LRule input output) : rest) c = if c == input then output else findRule rest c
