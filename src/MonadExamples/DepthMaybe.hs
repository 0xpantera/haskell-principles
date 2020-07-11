module MonadExamples.DepthMaybe where

import Text.Read

doubleStrNumber :: (Read a, Num a) => String -> Maybe a
doubleStrNumber str =
    case readMaybe str of
        Just x -> Just (2*x)
        Nothing -> Nothing

doubleStrNumber' :: (Read a, Num a) => String -> Maybe a
doubleStrNumber' str = fmap (*2) $ readMaybe str

doubleStrNumber'' :: (Read a, Num a) => String -> Maybe a
doubleStrNumber'' str =
    readMaybe str >>= 
        \x -> pure (2*x)

plusStrNumbers :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers x y = (+) <$> readMaybe x <*> readMaybe y

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName pn locs name = do
    phone <- lookup name pn
    loc <- lookup phone locs
    pure loc

locateByName' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName' pn locs name =
    lookup name pn >>= 
        \phone -> lookup phone locs >>=
            \loc -> pure loc

locateByName'' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName'' pn locs name =
    lookup name pn >>= flip lookup locs


