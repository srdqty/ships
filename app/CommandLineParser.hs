module CommandLineParser
    ( Options
    , parseCommandLine
    , getIPS
    , getInput
    , putOutput
    ) where

-------------------------------------------------------------------------------
import Data.Semigroup ((<>))

-------------------------------------------------------------------------------
import Data.Binary (decodeFile)
import qualified Data.ByteString.Lazy as L
    ( ByteString
    , getContents
    , readFile
    , putStr
    , writeFile
    )
import Options.Applicative
    ( (<*>)
    , (<|>)
    , Parser
    , ParserInfo
    , auto
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , infoOption
    , long
    , metavar
    , option
    , progDesc
    , short
    , strOption
    )

-------------------------------------------------------------------------------
import Data.Binary.IPS (IPS)

-------------------------------------------------------------------------------
data Input = Stdin | InFile FilePath

data Output = Stdout | OutFile FilePath

data Options = Options FilePath Input Output

-------------------------------------------------------------------------------
getIPS :: Options -> IO IPS
getIPS (Options filename _ _) = decodeFile filename

-------------------------------------------------------------------------------
putOutput :: Options -> L.ByteString -> IO ()
putOutput (Options _ _ Stdout) = L.putStr
putOutput (Options _ _ (OutFile name)) = L.writeFile name

-------------------------------------------------------------------------------
getInput :: Options -> IO L.ByteString
getInput (Options _ Stdin _) = L.getContents
getInput (Options _ (InFile name) _) = L.readFile name

-------------------------------------------------------------------------------
parseCommandLine :: String -> String -> IO Options
parseCommandLine version commitHash =
    execParser $ parserInfo version commitHash

-------------------------------------------------------------------------------
parserInfo :: String -> String -> ParserInfo Options
parserInfo versionString commitHash = info options description where
    description =
        fullDesc <> progDesc "ships -- apply IPS patch" <> header "sips"
    options = helper <*> versionOption <*> optionsParser
    versionOption = infoOption
        (versionString <> " " <> commitHash)
        (long "version" <> short 'v' <> help "Show version")

-------------------------------------------------------------------------------
optionsParser :: Parser Options
optionsParser = Options <$> ipsFilenameParser <*> inputParser <*> outputParser

-------------------------------------------------------------------------------
inputParser :: Parser Input
inputParser = InFile <$> parseInFile <|> pure Stdin where
    parseInFile = strOption $
           long "in-file"
        <> short 'i'
        <> metavar "INPUT_FILENAME"
        <> help "Optional input filename. Otherwise reads stdin"

-------------------------------------------------------------------------------
outputParser :: Parser Output
outputParser = OutFile <$> parseOutFile <|> pure Stdout where
    parseOutFile = strOption $
           long "out-file"
        <> short 'o'
        <> metavar "OUTPUT_FILENAME"
        <> help "Optional input filename. Otherwise reads stdin"

-------------------------------------------------------------------------------
ipsFilenameParser :: Parser String
ipsFilenameParser = strOption $
        long "ips-filename"
    <> short 'p'
    <> metavar "IPS_FILENAME"
    <> help "Name of IPS file to read and use for patching"
