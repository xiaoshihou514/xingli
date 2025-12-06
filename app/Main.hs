module Main where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Expr.LC qualified as LC
import Expr.LCN qualified as LCN
import Expr.LCNR qualified as LCNR
import Expr.ML qualified as ML
import Options.Applicative
import Parser.LC ()
import Parser.LCN ()
import Parser.LCNR ()
import Parser.ML ()
import System.Environment (lookupEnv)
import Typeclasses
import Types.ML (MLType (..))

data Input
  = FileInput FilePath
  | StdInput

data Locale = EnUS | ZhCN deriving (Eq)

data HelpText = HelpText
  { fileMeta :: String,
    fileHelp :: String,
    stdinHelp :: String,
    langMeta :: String,
    langHelp :: String,
    progDescription :: String,
    headerText :: String
  }

helpText :: Locale -> HelpText
helpText EnUS =
  HelpText
    { fileMeta = "FILENAME",
      fileHelp = "Input file",
      stdinHelp = "Read from stdin",
      langMeta = "language",
      langHelp = "Language (lc, lcn, lcnr, ml)",
      progDescription = "Type LC, LCN, LCNR or ML",
      headerText = "xingli - educational lambda calculus typer"
    }
helpText ZhCN =
  HelpText
    { fileMeta = "文件名",
      fileHelp = "输入文件",
      stdinHelp = "使用标准输入",
      langMeta = "语言",
      langHelp = "语言（LC, LCN, LCNR, ML）",
      progDescription = "选择表达式的语言",
      headerText = "型理 - 教学用λ演算类型推导器"
    }

fileInput :: HelpText -> Parser Input
fileInput txt =
  FileInput
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar (fileMeta txt)
          <> help (fileHelp txt)
      )

stdInput :: HelpText -> Parser Input
stdInput txt =
  flag'
    StdInput
    ( long "stdin"
        <> help (stdinHelp txt)
    )

data Opts = Opts
  { source :: Input,
    lang :: Maybe String
  }

optparse :: HelpText -> Parser Opts
optparse txt =
  let input :: Parser Input
      input = fileInput txt <|> stdInput txt
   in Opts
        <$> input
        <*> optional
          ( strOption
              ( long "lang"
                  <> metavar (langMeta txt)
                  <> help (langHelp txt)
              )
          )

getopt :: HelpText -> ParserInfo Opts
getopt txt =
  info
    (optparse txt <**> helper)
    ( fullDesc
        <> progDesc (progDescription txt)
        <> header (headerText txt)
    )

main :: IO ()
main = do
  locale <- detectLocale
  let txt = helpText locale
  customExecParser (prefs showHelpOnEmpty) (getopt txt) >>= run

run :: Opts -> IO ()
run (Opts src lang) = do
  input <- case src of
    FileInput path -> readFile path
    StdInput -> getContents

  let extLang = case src of
        FileInput path -> langFromExt path
        StdInput -> Nothing
  let language = decideLang lang extLang input
  let result = case language of
        "lc" -> prettyType $ snd <$> LC.pp (parse (init input) :: LC.Term)
        "lcn" -> prettyType $ snd <$> LCN.ppln (parse input :: LCN.LCNProgram)
        "lcnr" -> prettyType $ sndOf3 <$> LCNR.ppln (parse input :: LCNR.LCNRProgram)
        "ml" -> prettyType $ ML.algorithmW defaultMLEnv (parse input :: ML.MLTerm)
        _ -> "Unknown language. Use one of: lc, lcn, lcnr, ml."

  typed <- try $ evaluate result
  case typed of
    Left (err :: SomeException) -> putStrLn $ "Error: " ++ displayException err
    Right msg -> putStrLn msg
  where
    prettyType :: (Pretty a) => Maybe a -> String
    prettyType = maybe "Type error" pretty

    decideLang :: Maybe String -> Maybe String -> String -> String
    decideLang langOpt extOpt content =
      case fmap normalize langOpt <|> fmap normalize extOpt of
        Just l -> l
        Nothing
          | any (isPrefixOf "let ") ls -> "ml"
          | any (isPrefixOf "rec ") ls -> "lcnr"
          | length ls > 1 && any ('=' `elem`) ls -> "lcn"
          | otherwise -> "lc"
      where
        ls = map (dropWhile isSpace) $ filter (not . null) $ lines content
        normalize = map toLower

    langFromExt :: FilePath -> Maybe String
    langFromExt path =
      case map toLower (takeExtension' path) of
        ".lc" -> Just "lc"
        ".lcn" -> Just "lcn"
        ".lcnr" -> Just "lcnr"
        ".ml" -> Just "ml"
        _ -> Nothing

    -- Minimal replacement for System.FilePath.takeExtension without extra deps
    takeExtension' :: FilePath -> String
    takeExtension' p =
      case break (== '.') revSeg of
        (_, "") -> ""
        (revExt, '.' : _) -> '.' : reverse revExt
        _ -> ""
      where
        revSeg = takeWhile (`notElem` ['/', '\\']) (reverse p)

    defaultMLEnv :: Map.Map String MLType
    defaultMLEnv =
      Map.fromList
        [ ("true", Basic "Bool"),
          ("false", Basic "Bool"),
          ("zero", Basic "Int"),
          ("one", Basic "Int"),
          ("succ", Arrow (Basic "Int") (Basic "Int")),
          ("pred", Arrow (Basic "Int") (Basic "Int")),
          ("iszero", Arrow (Basic "Int") (Basic "Bool")),
          ("not", Arrow (Basic "Bool") (Basic "Bool")),
          ("and", Arrow (Basic "Bool") (Arrow (Basic "Bool") (Basic "Bool"))),
          ("or", Arrow (Basic "Bool") (Arrow (Basic "Bool") (Basic "Bool"))),
          ("+", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
          ("*", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
          ("-", Arrow (Basic "Int") (Arrow (Basic "Int") (Basic "Int"))),
          ("if", Qtf 'a' (Arrow (Basic "Bool") (Arrow (Phi 'a') (Arrow (Phi 'a') (Phi 'a'))))),
          ("Nil", Qtf 'a' (Basic "List")),
          ("Cons", Qtf 'a' (Arrow (Phi 'a') (Arrow (Basic "List") (Basic "List")))),
          ("isNil", Qtf 'a' (Arrow (Basic "List") (Basic "Bool"))),
          ("head", Qtf 'a' (Arrow (Basic "List") (Phi 'a'))),
          ("tail", Qtf 'a' (Arrow (Basic "List") (Basic "List")))
        ]

detectLocale :: IO Locale
detectLocale = do
  envs <- mapM lookupEnv ["LC_ALL", "LANGUAGE", "LC_MESSAGES", "LANG"]
  let lang' = map toLower <$> firstJust envs
  return $
    case lang' of
      Just l | "zh" `isPrefixOf` l -> ZhCN
      _ -> EnUS
  where
    firstJust :: [Maybe a] -> Maybe a
    firstJust = listToMaybe . catMaybes

sndOf3 :: (a, b, c) -> b
sndOf3 (_, y, _) = y
