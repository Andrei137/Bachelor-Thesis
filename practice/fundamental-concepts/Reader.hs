import Data.Char as Char

-- Our config data. We're very particular about certain
-- letters of the alphabet, see.
data ABConfig = ABConfig
    { don'tUseLetterE :: Bool
    , don'tUseLetterL :: Bool
    }
    deriving (Show)

-- Uppercase the string, obeying current tests.
toUpperStr' :: ABConfig -> String -> String
toUpperStr' cfg str =
    filter passesFilters (fmap Char.toUpper str)
    where
        filters :: [Char -> Bool]
        filters =
            [ if don'tUseLetterE cfg then (/= 'E') else const True
            , if don'tUseLetterL cfg then (/= 'L') else const True
            ]

        passesFilters :: Char -> Bool
        passesFilters c = all (\f -> f c) filters

welcomeMessage' :: ABConfig -> String -> String -> String
welcomeMessage' cfg motd username =
    "Welcome, " ++
    toUpperStr' cfg username ++
    "! Message of the day: " ++
    toUpperStr' cfg motd

fullName' :: ABConfig -> String -> String -> String -> String
fullName' cfg firstname nickname lastname =
    toUpperStr' cfg firstname ++ " \"" ++
    toUpperStr' cfg nickname ++ "\" " ++
    toUpperStr' cfg lastname

{-
    :: ABConfig -> String -> String
    :: ABConfig -> String -> String -> String
    :: ABConfig -> String -> String -> String -> String
    :: config -> a
-}

newtype Reader cfg a = Reader { runReader :: cfg -> a }

instance Functor (Reader cfg) where
    fmap f ra = Reader $ \cfg -> f $ runReader ra cfg

instance Applicative (Reader cfg) where
    pure a = Reader $ const a
    rf <*> ra = Reader $ \cfg -> (runReader rf cfg) (runReader ra cfg)

instance Monad (Reader cfg) where
    return = pure
    ra >>= f = Reader $ \cfg -> runReader (f $ runReader ra cfg) cfg

-- Ask for the whole config
ask :: Reader cfg cfg
ask = Reader id

-- Ask for a specific part of the config
asks :: (cfg -> a) -> Reader cfg a
asks = Reader

toUpperStr :: String -> Reader ABConfig String
toUpperStr str = do
    cfg <- ask
    let filters :: [Char -> Bool]
        filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
                  , if don'tUseLetterL cfg then (/= 'L') else const True
                  ]
        passesFilters :: Char -> Bool
        passesFilters c = all (\f -> f c) filters
    return . filter passesFilters $ fmap Char.toUpper str


welcomeMessage :: String -> String -> Reader ABConfig String
welcomeMessage motd username = do
    upperMotd <- toUpperStr motd
    upperUsername <- toUpperStr username
    return $ "Welcome, " ++ upperUsername ++ "! Message of the day: " ++ upperMotd

fullName :: String -> String -> String -> Reader ABConfig String
fullName firstname nickname lastname = do
    upperFirstname <- toUpperStr firstname
    upperNickname <- toUpperStr nickname
    upperLastname <- toUpperStr lastname
    return $ upperFirstname ++ " \"" ++ upperNickname ++ "\" " ++ upperLastname

-- Replaces the current config with a new one for the duration of the action
local :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local transform rf = Reader $ \cfg -> runReader rf $ transform cfg

{-
    transform :: cfg -> cfg'
    rf :: Reader cfg' function
    local :: Reader cfg function
    cfg :: cfg
    transform cfg :: cfg'
    runReader rf $ transform myCfg :: function
    local :: Reader cfg function
-}

localUsage :: Reader ABConfig String
localUsage = do
    cfg <- ask
    local (\c -> c { don'tUseLetterE = True }) $ toUpperStr "hello"
