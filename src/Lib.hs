module Lib where


data PersonRecord = PersonRecord { name    :: String
                                 , address :: Address
                                 , id      :: Integer
                                 , labels  :: [Label]
                                 } deriving Show

data Address = Address { line1  :: String
                       , number :: Integer
                       , street :: String
                       , town   :: String
                       } deriving Show

data Label = Green | Blue | Red | Yellow deriving Show

rec1 = PersonRecord
    "Wim Vanderbauwhede"
    (Address "School of Computing Science" 17 "Lilybank Gdns" "Glasgow")
    557188
    [Green, Red]

rec2 = PersonRecord
    "Jeremy Singer"
    (Address "School of Computing Science" 17 "Lilybank Gdns" "Glasgow")
    42
    [Blue, Yellow]
