{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as ByteString
import           Data.Monoid ((<>))
import qualified Data.Text.IO as Text
import           GitHub.Auth
import           GitHub.Data.Id
import           GitHub.Data.PullRequests
import           GitHub.Endpoints.PullRequests
import           Options.Applicative
import           System.Environment (lookupEnv)
import           System.IO (hPrint, stdin, stderr)

main :: IO ()
main = do
  target <- execParser opts
  auth <- fmap (OAuth . ByteString.pack) <$> lookupEnv "GITHUB_PANDOC_OAUTH"
  flip (maybe (Text.hPutStrLn stderr "No auth information!")) auth
    $ \auth -> do
      body <- Text.hGetContents stdin
      case target of
        TargetPullRequest owner repo id -> 
          (updatePullRequest auth owner repo id $ EditPullRequest Nothing (Just body) Nothing Nothing Nothing)
            >>= either (hPrint stderr) print

data GithubTarget =
  TargetPullRequest
  { targetPullRequestOwner :: Name Owner
  , targetPullRequestRepo  :: Name Repo
  , targetPullRequestId    :: Id PullRequest
  }

opts :: ParserInfo GithubTarget
opts =
  info
  (optionsParser <**> helper)
  (   fullDesc
   <> progDesc "Upload documentation to Github as Markdown"
   <> header "pandoc-github - Send pandoc markdown to github")

optionsParser :: Parser GithubTarget
optionsParser = pullRequestParser
  where
    pullRequestParser =
          TargetPullRequest
      <$> strOption
          (   long "owner"
           <> metavar "OWNER"
           <> help "The owner of the repository")
      <*> strOption
          (   long "repo"
           <> metavar "REPO"
           <> help "The repository for the pull request")
      <*> (Id <$>
           option auto
           (   long "id"
            <> metavar "ID"
            <> help "The ID of the pull request"))

