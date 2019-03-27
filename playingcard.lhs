======================================================================
CS457 - Project - Lucas Petherbridge
2019-02-21
======================================================================

Inspiration https://wiki.haskell.org/Type

> module PlayingCard where

> import Control.Monad
> import Data.Array.IO
> import System.Random
> import Data.List (intersperse)

======================================================================
Data Types
----------

> data Suit = Hearts | Diamonds | Clubs | Spades
>   deriving (Read, Enum, Eq, Ord)

> instance Show Suit where
>   show s = case s of
>     Hearts   -> "♡ ";  Diamonds -> "♢ "
>     Clubs    -> "♣";   Spades   -> "♠"

> data Rank =
>   Ace  | Two   | Three | Four | Five |
>   Six  | Seven | Eight | Nine | Ten  |
>   Jack | Queen | King
>   deriving (Read, Enum, Eq, Ord)

> instance Show Rank where
>   show r = case r of
>     Ace   -> "A";  Two   -> "2";  Three -> "3";  Four  -> "4"
>     Five  -> "5";  Six   -> "6";  Seven -> "7";  Eight -> "8"
>     Nine  -> "9";  Ten   -> "10"; Jack  -> "J";  Queen -> "Q"
>     King  -> "K"

> data Card = Card { rank   :: Rank,
>                    suit   :: Suit,
>                    faceUp :: Bool }
>   deriving (Read, Eq)

> instance Show Card where
>   show c = unlines $ ["┌─────┐"] ++ content ++ ["┕─────┘"]
>     where r    = show $ rank c;  s    = show $ suit c
>           res  = "\ESC[0m";      bclr = "\ESC[32m"
>           clr  = case (suit c) of
>                    Hearts   -> "\ESC[31m"  -- red
>                    Diamonds -> "\ESC[31m"  -- red
>                    Clubs    -> "\ESC[30m"  -- black
>                    Spades   -> "\ESC[30m"  -- black
>           -- Spacing to allow for the width of Rank 10
>           rsp = concat $ replicate (2-length r) " "
>           -- Spacing to allow for Heart and Diamond which are
>           -- length 2. Clubs and Spades are length 1
>           ssp = concat $ replicate (2-length s) " "
>           content = if faceUp c then
>               -- Edge, color, rank, spacing, suit, color reset, edge
>               [concat ["│", clr, r, rsp, " ", ssp, s, res, "│"],
>               -- Edge, spacing, edge
>               "│     │",
>               -- Edge, color, suit, spacing, rank, color reset, edge
>               concat ["│", clr, s, ssp, " ", rsp, r, res, "│"]]
>             -- Card back - edge, color, pattern, color reset, edge
>             else replicate 3 $ concat ["│ ", bclr, "≡≡≡", res, " │"]

> instance Ord Card where
>   compare c1 c2 = compare (rank c1, suit c1) (rank c2, suit c2)

> instance Enum Card where
>   toEnum n = let (r, s) = n `divMod` 4
>     in Card (toEnum r) (toEnum s) False
>   fromEnum c = fromEnum (rank c) * 4 + fromEnum (suit c)

> type Deck = [Card]

======================================================================
Card functions
--------------

> showEmpty :: String
> showEmpty  = unlines $
>   [concat [clr, "┌─────┐", res]] ++
>   (replicate 3 $ concat [clr, "│     │", res]) ++
>   [concat [clr, "└─────┘", res]]
>   where clr = "\ESC[30;1m"
>         res = "\ESC[0m"

> padPile :: Int -> String -> String
> padPile n s = let pad = concat $ replicate n " "
>               in unlines $ map (++pad) $ lines s

> showStackPile   :: Deck -> String
> showStackPile [] = padPile 3 showEmpty
> showStackPile (c:cs) =
>   if length cs > 0 then
>     unlines $ map join $ zip
>       (lines "┌\n│\n│\n│\n└") (lines $ padPile 2 $ show c)
>   else padPile 3 $ show c
>   where join (p1, p2) = concat [p1, p2]

> cardPad :: String
> cardPad  = concat $ replicate 10 " "
> defaultPad :: Int
> defaultPad  = 3

> showFlatPile       :: Deck -> String
> showFlatPile []     = padPile defaultPad showEmpty
> showFlatPile (c:cs) = padPile defaultPad $ show c

> flipCard     :: Card -> Card
> flipCard card = card { faceUp = not $ faceUp card }
> cardUp       :: Card -> Card
> cardUp card   = card { faceUp = True }
> cardDown     :: Card -> Card
> cardDown card = card { faceUp = False }

======================================================================
Deck functions
------------

> emptyDeck :: Deck
> emptyDeck  = []
> newDeck :: Deck
> newDeck  = [Card v s False | v <- [Ace .. King],
>                           s <- [Hearts .. Spades]]
> shuffledDeck :: IO Deck
> shuffledDeck  = shuffle newDeck

> -- Source https://wiki.haskell.org/Random_shuffle
> shuffle :: [a] -> IO [a]
> shuffle xs = do
>   ar <- newArray n xs
>   forM [1..n] $ \i -> do
>     j <- randomRIO (i,n)
>     vi <- readArray ar i
>     vj <- readArray ar j
>     writeArray ar j vi
>     return vj
>   where
>     n = length xs
>     newArray :: Int -> [a] -> IO (IOArray Int a)
>     newArray n xs =  newListArray (1,n) xs

======================================================================
Debug Display Functions
------------------------

> pileStr  :: [Deck] -> String
> pileStr p = (concat $ map deckStr p) ++ "\n"
> deckStr   :: Deck -> String
> deckStr [] = ""
> deckStr d  = (concat $ intersperse " " $ map cardStr d) ++ "\n"
> cardStr  :: Card -> String
> cardStr c = (show $ rank c) ++ (show $ suit c)
