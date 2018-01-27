import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate db = foldr f [] db
  where 
    f x acc = 
      case x of
        DbDate d -> d:acc 
        _ -> acc 


filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr f [] db
  where
    f x acc = 
      case x of
        DbNumber n -> n:acc
        _ -> acc


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = maximum . filterDbDate $ db


sumDb :: [DatabaseItem] -> Integer
sumDb db = sum . filterDbNumber $ db


avgDb :: [DatabaseItem] -> Double 
avgDb db = (/ (fromInteger . toInteger . length . filterDbNumber $ db)) (fromInteger . toInteger . sumDb $ db)
