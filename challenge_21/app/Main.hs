-- Data Entry
-- Create a program that retrieves the membership details for a Rock Climbing Club.

module Main where

import Chronos (SubsecondPrecision (SubsecondPrecisionFixed), Time (..), encode_DmyHMS, now, slash, timeToDatetime)
import Control.Exception (IOException, try)
import Control.Monad (forM_)
import qualified Data.Bifunctor (second)
import Data.Binary (Binary (get, put))
import qualified Data.Binary (decodeFileOrFail, encodeFile)
import Data.Binary.Get (ByteOffset)
import Options.Applicative
import System.Directory (doesFileExist)
import Text.Printf (printf)

data Args = Args String Command deriving (Show)

data Command = Add Member | List | New Bool | Remove Int deriving (Show)

data Member = Member
        { firstName :: String
        , lastName :: String
        , registrationTime :: Time
        }
        deriving (Show)

instance Binary Member where
        put member = do
                put $ firstName member
                put $ lastName member
                put $ getTime $ registrationTime member
        get = do
                firstName' <- get
                lastName' <- get
                registrationTime' <- get
                return Member{firstName = firstName', lastName = lastName', registrationTime = Time registrationTime'}

argsParser :: Time -> ParserInfo Args
argsParser time =
        info
                ( Args
                        <$> option auto (long "file" <> short 'f' <> metavar "FILENAME" <> showDefault <> value "members.db")
                        <*> subparser
                                ( command "add" (info (Add <$> member <**> helper) (progDesc "Add member"))
                                        <> command "ls" (info (pure List <**> helper) (progDesc "List members"))
                                        <> command "rm" (info (Remove <$> argument auto (metavar "INDEX") <**> helper) (progDesc "Remove a member"))
                                        <> command "new-db" (info (New <$> switch (long "overwrite") <**> helper) (progDesc "Create new membership database"))
                                )
                                <**> helper
                )
                (progDesc "Manage memberships")
    where
        member = Member <$> strArgument (metavar "FIRST-NAME") <*> strArgument (metavar "LAST-NAME") <*> pure time

removeAt :: (Integral t) => t -> [a] -> Maybe (a, [a])
removeAt 1 (x : xs) = Just (x, xs)
removeAt n (x : xs) | n >= 1 = Data.Bifunctor.second (x :) <$> removeAt (n - 1) xs
removeAt _ _ = Nothing

encodeFile :: (Binary a) => FilePath -> a -> IO (Maybe IOException)
encodeFile path val = do
        err <- try $ Data.Binary.encodeFile path val
        return
                ( case err of
                        Left err' -> Just err'
                        Right _ -> Nothing
                )

data DecodeFileError = DecodeFileErrorIO IOException | DecodeFileErrorParse ByteOffset String

main :: IO ()
main = do
        time <- now
        Args path cmd <- execParser $ argsParser time
        case cmd of
                Add member -> withDecodeMembers path (add path member)
                List -> withDecodeMembers path list
                New overwrite -> new path overwrite
                Remove index -> withDecodeMembers path (remove path index)
    where
        fmtMember member = printf "%s %s (registered %s)" (firstName member) (lastName member) (fmtRegistrationTime member)
        fmtRegistrationTime = encode_DmyHMS (SubsecondPrecisionFixed 0) slash . timeToDatetime . registrationTime

        withDecodeMembers path f = do
                result <- try $ Data.Binary.decodeFileOrFail path
                case result of
                        Right (Right members') -> f members'
                        Right (Left (offset, message)) -> printf "Error: failed to decode file: %s (offset %s)\n" message (show offset)
                        Left err -> putStrLn $ "Error: failed to open file: " ++ show (err :: IOError)

        new path overwrite = do
                exists <- doesFileExist path
                if not overwrite && exists
                        then printf "Error: %s already exists\n" path
                        else do
                                err <- encodeFile path ([] :: [Member])
                                case err of
                                        Just _ -> putStrLn "Error: failed to write file"
                                        Nothing -> printf "Created %s\n" path

        add path member members = do
                err <- encodeFile path (member : members)
                case err of
                        Just _ -> putStrLn "Error: failed to write file"
                        Nothing -> putStrLn $ "Added " ++ fmtMember member

        remove path index members = case removeAt index members of
                Just (member, members') -> do
                        err <- encodeFile path members'
                        case err of
                                Just _ -> putStrLn "Error: failed to write file"
                                Nothing -> putStrLn $ "Removed " ++ fmtMember member
                Nothing -> putStrLn "Error: invalid index"

        list :: [Member] -> IO ()
        list members =
                forM_
                        (zip [1 ..] members)
                        ( \(index, member) ->
                                printf
                                        "%i. %s\n"
                                        (index :: Int)
                                        (fmtMember member :: String)
                        )
