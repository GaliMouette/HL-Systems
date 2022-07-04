module LOptions (LOptions (..), programLOptions) where

import Options.Applicative
  ( Parser,
    ParserInfo,
    auto,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    showDefault,
    strOption,
    switch,
    value,
    (<**>),
  )

data LOptions = LOptions
  { gen :: Int,
    anim :: Bool,
    -- param
    file :: String
  }

programLOptions :: ParserInfo LOptions
programLOptions =
  info
    (lOptions <**> helper)
    ( fullDesc
        <> header "A L-System drawing tool"
        <> progDesc "Draw L-Systems from a set of rules and axioms"
    )

lOptions :: Parser LOptions
lOptions = LOptions <$> genLOption <*> animLOption <*> fileLOption

genLOption :: Parser Int
genLOption =
  option
    auto
    ( long "generation"
        <> help "The number of generation to compute, if 0 or not specified then the program doesn't stop"
        <> showDefault
        <> value 0
        <> metavar "GEN"
    )

animLOption :: Parser Bool
animLOption = switch (long "animate" <> help "Whether to animate")

fileLOption :: Parser String
fileLOption =
  strOption
    ( long "file"
        <> help "Which file to use as input"
        <> metavar "FILE"
    )
