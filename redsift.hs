{-# language OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<|>))
import Data.Aeson (ToJSON(..), encode)
import Data.ByteString (ByteString)
import Data.String.Conversions
import Database.PostgreSQL.Simple hiding (query, Query)
import Filesystem.Path.CurrentOS (decodeString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp hiding (Connection)
import Network.Wai.Application.Static
import Network.Wai.UrlMap
import System.FilePath
import System.IO
import Control.Exception (bracket)

import Paths_redsift

import Redsift.DB
import Redsift.Exception
import Redsift.Config

main :: IO ()
main = do
    redsiftConfig <- readRedsiftConfig
    let port = appPort redsiftConfig
        connectInfo = defaultConnectInfo {
            connectHost = dbHost redsiftConfig,
            connectPort = fromIntegral (dbPort redsiftConfig),
            connectUser = dbUser redsiftConfig,
            connectDatabase = dbName redsiftConfig}
    documentRoot <- (</> "www") <$> getDataDir
    hPutStrLn stderr ("attempting to listen on port " ++ show port)
    run port $ handleApp errorHandler $
        mapUrls (redsiftApp connectInfo redsiftConfig documentRoot)

-- | Routing between static files and the API
redsiftApp :: ConnectInfo -> RedsiftConfig -> FilePath -> UrlMap
redsiftApp connectInfo redsiftConfig documentRoot =
    mount "api" (apiApp connectInfo redsiftConfig) <|>
    mountRoot (fileServerApp documentRoot)

-- * file serving
fileServerApp :: FilePath -> Application
fileServerApp documentRoot =
    staticApp (defaultFileServerSettings (decodeString (documentRoot ++ "/")))

-- * api
apiApp :: ConnectInfo -> RedsiftConfig -> Application
apiApp connectInfo redsiftConfig request =
    case requestMethod request of
        "GET" -> case pathInfo request of
            ["table", "list"] ->
                withDB connectInfo $ \ db -> do
                    tables <- allTables db
                    return $ responseLBS ok200 [] (encode (toJSON tables))
            ["query"] -> queryVarRequired (queryString request) "q" $ \ q ->
                withDB connectInfo $ \ db -> do
                    result <- query db (cs q) redsiftConfig
                    return $ responseLBS ok200 [] (encode (toJSON result))
            ["export"] -> queryVarRequired (queryString request) "e" $ \ e ->
                withDB connectInfo $ \ db -> do
                    queryVarRequired (queryString request) "n" $ \ n -> do
                        result <- export db (getEmail request) (cs n) (cs e) redsiftConfig
                        return $ responseLBS ok200 [] (encode (toJSON result))
            _ -> return notFoundError
        _ -> return notFoundError
    where getEmail request = cs $ snd $ head $ filter (\header -> fst header == "From") (requestHeaders request)
          notFoundError = responseLBS notFound404 [] "404 not found"

queryVarRequired :: Query -> ByteString -> (ByteString -> IO Response) -> IO Response
queryVarRequired query key cont = case lookup key query of
    Just (Just value) -> cont value
    _ -> return $ responseLBS badRequest400 [] ("missing query var: " <> cs key)

-- Make sure a connection is closed after we finished with the query.
withDB :: ConnectInfo -> (Connection -> IO a) -> IO a
withDB connectInfo = bracket (connect connectInfo) close