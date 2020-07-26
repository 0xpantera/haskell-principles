module InDepth.DMaybe where

import Text.Read (readMaybe)

doubleStrNumber1 :: (Read a, Num a) => String -> Maybe a
doubleStrNumber1 str =
    case (readMaybe str) of
        Nothing -> Nothing
        Just s -> Just (2*s)

doubleStrNumber2 :: (Num a, Read a) => String -> Maybe a
doubleStrNumber2 str =
    fmap (2*) $ readMaybe str

doubleStrNumber3 :: (Num a, Read a) => String -> Maybe a
doubleStrNumber3 str =
    readMaybe str >>= \x -> return $ x*2

doubleStrNumber4 :: (Num a, Read a) => String -> Maybe a
doubleStrNumber4 str = do
    x <- readMaybe str
    return $ x*2

plusStrNumbers :: (Num a, Read a) => String -> String -> Maybe a
plusStrNumbers s1 s2 = (+) <$> readMaybe s1 <*> readMaybe s2

type Name = String
type Phone = String
type Location = String
type PhoneNumbers = [(Name, Phone)]
type Locations = [(Phone, Location)]

locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName pn locs n =
    lookup n pn >>= flip lookup locs

locateByName2 :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName2 pn locs n =
    case (lookup n pn) of
        Just phone -> lookup phone locs
        Nothing -> Nothing

