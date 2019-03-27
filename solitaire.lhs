======================================================================
CS457 - Project - Lucas Petherbridge
2019-02-21
======================================================================

> module Solitaire where

> import PlayingCard
> import Data.Char (toLower, toUpper)
> import Data.List (intersperse, zip7)
> import Text.Read (readMaybe)
> import System.Console.Haskeline
> import Control.Monad.IO.Class

======================================================================
Data Types
----------

> data Board = Board {
>   tableau     :: [Deck],  -- Column piles
>   foundations :: [Deck],  -- Suit/Sequence piles
>   stock       :: Deck,    -- Draw or Hand pile
>   talon       :: Deck,    -- Waste pile
>   message     :: String,  -- Used for errors or other messages
>   debug       :: Bool     -- Allows extra options when True
> }

> instance Show Board where
>   show b = unlines $ headerLabels  ++ headerRow ++
>                      tableauLabels ++ tableauRows
>     where
>       headerLabels  = hPad ["  ","W ","  ","F1","F2","F3","F4"]
>       tableauLabels = hPad ["T1","T2","T3","T4","T5","T6","T7"]
>       hPad l = [concat $ map (\x -> " " ++ x ++ take 7 cardPad) l]
>       -- Interleave the stock, talon, and foundation piles
>       headerRow = map join7 $ zip7 st tl sp f1 f2 f3 f4
>       ds = map (lines . showStackPile) [stock b, talon b]
>       st = ds!!0;  tl = ds!!1;
>       sp = replicate 5 cardPad -- Blank column
>       fs = map (lines . showFlatPile) $ foundations b
>       f1 = fs!!0;  f2 = fs!!1;  f3 = fs!!2;  f4 = fs!!3
>       -- Interleave the tableau columns
>       tableauRows = map join7 $ zip7 t1 t2 t3 t4 t5 t6 t7
>       ts = map tPile $ tableau b
>       t1 = ts!!0;  t2 = ts!!1;  t3 = ts!!2;  t4 = ts!!3
>       t5 = ts!!4;  t6 = ts!!5;  t7 = ts!!6
>       join7 (p1, p2, p3, p4, p5, p6, p7) = concat
>         [p1, p2, p3, p4, p5, p6, p7]
>       -- tPile pads each vertical tableau column with space
>       -- based on the max height of all columns
>       tPile p = (lines $ showTableau $ reverse p) ++
>                 replicate pad cardPad
>         where lenP = length p
>               maxT = maximum $ map length $ tableau b
>               pad  = if maxT >= lenP then 5 * (maxT-lenP)
>                                      else 0
>       showTableau   :: Deck -> String
>       showTableau [] = padPile defaultPad showEmpty
>       showTableau d  = (concat $ map top $ init d) ++
>           (padPile defaultPad $ show $ last d)
>         where top c = unlines $ take 2 $ lines $ padPile defaultPad $ show c

> data Move =
>   InvalidMove |
>   Move {
>     mCount :: Int,   -- Number of cards to move
>     mFrom  :: Pile,  -- Which pile to move from
>     mTo    :: Pile   -- Which pile to move to
>   }
>   deriving (Read, Show, Eq)

> data Pile =
>   W  | T1 | T2 | T3 | T4 | T5 | T6 | T7 |
>   F1 | F2 | F3 | F4
>   deriving (Read, Show, Eq, Ord, Enum)
> data PileType = Talon | Tableau | Foundation
>   deriving (Read, Show, Eq)

======================================================================
Set up our game and then start main interactive program
-------------------------------------------------------

> main :: IO ()
> main = newGame True False

> newGame :: Bool -> Bool -> IO ()
> newGame shuff debug = do
>   board <- newBoard shuff debug
>   runInputT defaultSettings $ mip board

> debugGame :: IO ()
> debugGame  = newGame True True
> nonShuffGame :: IO ()
> nonShuffGame  = newGame False True

> newBoard :: Bool -> Bool -> IO Board
> newBoard shuff debug = do
>   shuffDeck <- shuffledDeck
>   let deck        = if shuff then shuffDeck else newDeck
>       tableau     = dealTableau deck
>       foundations = emptyFoundationList
>       -- Put the remaining items not dealt into the stock
>       stock       = drop (sum $ map length tableau) deck
>       talon       = emptyDeck
>       message     = "Let's play a game of Solitaire!\n" ++
>                     "Type 'h' for help."
>   return $ Board tableau foundations stock talon message debug

> mip :: Board -> InputT IO ()
> mip board = do
>   outputStr "\ESC[2J\ESC[1;1;H"  -- Clear the screen
>   outputStrLn $ show board
>   -- Count cards in Foundations - 52 means winner!
>   let fs = sum $ map length $ foundations board
>   if fs == length newDeck then do
>     playAgain $ setMsg board "You Win!!!"
>   else do
>     let m = message board
>     if (length m) > 0 then outputStrLn m
>                       else outputStr ""
>     line <- getInputLine "Command: "
>     case line of
>       Just line ->
>         case line of
>           []     -> mip board
>           (o:os) -> case toLower o of
>             'h' -> mip $ setMsg board help  -- Print help
>             'd' -> mip $ drawCard board  -- Draw a card
>             'm' ->  -- Move card(s)
>               let params = words os
>               in case length params of
>                 2 -> let (f, t) = ftParams params 0 1
>                      in mip $ snd $ moveCards "" f t board
>                 3 -> let n      = params !! 0
>                          (f, t) = ftParams params 1 2
>                      in mip $ snd $ moveCards n f t board
>                 _ -> mip $ setErr board "invalid move."
>               where ftParams s n m = (map toUpper $ s !! n,
>                                       map toUpper $ s !! m)
>             'a' -> mip $ autoMove board  -- Auto move to foundation
>             -- Start new game
>             's' -> do board <- liftIO $ newBoard True False
>                       mip board
>             'q' -> return ()  -- Quit game
>             't' -> if (debug board) then
>                      mip $ setMsg board $ boardStr board -- DEBUG
>                    else
>                      mip $ setErr board "unrecognized option."
>             _   -> mip $ setErr board "unrecognized option."
>       Nothing -> mip board
>   where boardStr  :: Board -> String
>         boardStr b = "Stock: \n" ++ (deckStr $ stock b) ++
>                      "Talon: \n" ++ (deckStr $ talon b) ++
>                      "Tableau: \n" ++ (pileStr $ tableau b) ++
>                      "Foundations: \n" ++ (pileStr $ foundations b)

> playAgain      :: Board -> InputT IO ()
> playAgain board = do
>   outputStr "\ESC[2J\ESC[1;1;H"  -- Clear the screen
>   outputStrLn $ show board
>   outputStrLn $ message board
>   line <- getInputLine "Play again? (y/n): "
>   case line of
>     Just line ->
>       case line of
>         [] -> playAgain board
>         (o:os) -> case toLower o of
>           -- Start new game
>           'y' -> do board <- liftIO $ newBoard True False
>                     mip board
>           'n' -> return () -- Quit game
>           _   -> playAgain $ setErr board "invalid choice."
>     Nothing   -> playAgain board

======================================================================
Initializers/Constants
----------------------

> help :: String
> help  =
>   "Terminology:\n" ++
>   "  The Tableau: Seven piles that make up the main table. " ++
>     "(labeled T1 - T7)\n" ++
>   "  The Foundations: Four piles on which a whole suit must be " ++
>     "built up in sequence from Ace to King. (labeled F1 - F4)\n" ++
>   "  The Stock (or \"Hand\") Pile: The main deck containing the " ++
>     "remaining cards to draw from.\n" ++
>   "  The Talon (or \"Waste\") Pile: Cards drawn from the Stock " ++
>     "that have no place in the Tableau. (labeled W)\n\n" ++
>   "Command options:\n" ++
>   "  d : Draw a card from the Stock\n" ++
>   "  m : Move a card. Requires 2-3 parameters: n, from, and to.\n" ++
>   "      Parameters are case-insensitive so T1 == t1.\n" ++
>   "      e.g. \"m 2 T1 T2\" : Move 2 cards from Tableau 1 to " ++
>               "Tableau 2\n" ++
>   "           \"m T2 F1\"   : Move all valid cards from Tableau 1 " ++
>               "to Foundation 1\n\n" ++
>   "      n    : Optional: The number of cards to move. Can only " ++
>                 "move cards that are face up.\n" ++
>   "             If not provided, the maximum number of cards " ++
>                 "will be moved.\n" ++
>   "      from : Where to move card(s) from.\n" ++
>   "             W     : The Talon (or \"Waste\") Pile\n" ++
>   "             T1-T7 : The Tableau\n" ++
>   "             F1-F4 : The Foundations\n" ++
>   "      to   : Where to move card(s) to.\n" ++
>   "             T1-T7 : The Tableau\n" ++
>   "             F1-F4 : The Foundations\n" ++
>   "  a : Auto move cards. Attempts to move all available cards\n" ++
>   "      in ordered sequence to the Foundation piles.\n"

> maxn :: Int
> maxn  = 12

> emptyErr  :: Pile -> String
> emptyErr p = concat [show p, " is empty"]
> countErr    :: Int -> Pile -> String
> countErr n p = let plur = if n == 1 then ""
>                                     else "s"
>   in concat ["can only move ", show n, " card", plur,
>              " from ", show p, "."]
> rankErr    :: Rank -> Rank -> String
> rankErr a b = concat [show a, " can not be placed on a ",
>                       show b, "."]
> suitRankErr    :: Card -> Card -> String
> suitRankErr a b = concat [show $ rank a, show $ suit a,
>                           " can not be placed on a ",
>                           show $ rank b, show $ suit b, "."]
> kingErr  :: Pile -> String
> kingErr p = concat ["only a King can be placed on ", show p, "."]
> aceErr  :: Pile -> String
> aceErr p = concat ["only an Ace can be placed on ", show p, "."]

> emptyFoundationList :: [Deck]
> emptyFoundationList  = replicate 4 emptyDeck

> -- Bulds up a triangular list of lists and flips the first one face
> -- up
> dealTableau  :: Deck -> [Deck]
> dealTableau d = [flipTableau $ take n $ drop (sum [1..n-1]) d |
>                  n <- [1..7]]

======================================================================
Board Actions
-------------

> setMsg    :: Board -> String -> Board
> setMsg b s = b { message = s }

> setErr    :: Board -> String -> Board
> setErr b s = setMsg b ("Error: " ++ s)

> -- Flips the first card of a deck face up
> flipTableau       :: Deck -> Deck
> flipTableau []     = []
> flipTableau (c:cs) = cardUp c : cs

> drawCard  :: Board -> Board
> drawCard b = let st = stock b
>   in if length st > 0 then
>        b { stock = drop 1 st,
>            talon = (cardUp $ head st) : talon b,
>            message = "" }
>      else
>        setErr b "Stock pile is empty!"

> autoMove      :: Board -> Board
> autoMove board =
>   let board' = if (length $ talon board) == 0 then drawCard board
>                else board
>   in tryMoves moves board' False
>   where moves = [(f,t) | f <- [W .. T7], t <- [F1 .. F4]]
>         tryMoves :: [(Pile, Pile)] -> Board -> Bool -> Board
>         tryMoves [] b succ =
>           if succ then autoMove b
>                   else b { message = "" }
>         tryMoves (m:ms) b succ =
>           let from        = show $ fst m
>               to          = show $ snd m
>               (succ', b') = moveCards "1" from to b
>               succ''      = if succ then True else succ'
>           in tryMoves ms b' succ''

> moveCards :: String -> String -> String -> Board -> (Bool, Board)
> moveCards n f t board = let (move, err) = parseMove n f t
>   in if move == InvalidMove then (False, setErr board err)
>      else
>        let from = mFrom move
>            to   = mTo move
>            n    = mCount move
>            (valid, err) = validFromMove n from board
>        in if valid then
>            let (valid', err') = validToMove n from to board
>            in if valid' then
>                 let (cards, board') = takeCards n from board
>                     board'' = addCards cards to board'
>                 in (True, board'')
>               else (False, setErr board err')
>           else (False, setErr board err)

> validFromMove :: Int -> Pile -> Board -> (Bool, String)
> validFromMove n from board =
>   let tl = talon board
>       ts = tableau board
>       fs = foundations board
>   in case parsePile from of
>        Talon -> -- Can only take 1 card from the Talon
>          if length tl > 0 then
>            if n == 1 || n == maxn then (True, "")
>            else (False, countErr 1 from)
>          else (False, emptyErr from)
>        Tableau -> -- Can only take <= the number of face up cards
>          let idx = fromEnum from - 1
>              faceCount = length $ filter (\x -> faceUp x) $ ts!!idx
>          in if faceCount > 0 then
>               if n <= faceCount || n == maxn then (True, "")
>               else (False, countErr faceCount from)
>             else (False, emptyErr from)
>        Foundation -> -- Can only take <= the number of cards
>          let idx = fromEnum from - 8
>              cardCount = length $ fs !! idx
>          in if cardCount > 0 then
>               if n <= cardCount || n == maxn then (True, "")
>               else (False, countErr cardCount from)
>             else (False, emptyErr from)

> validToMove :: Int -> Pile -> Pile -> Board -> (Bool, String)
> validToMove n from to b =
>   let fCard  = last $ fst $ takeCards n from b
>       fRank  = rank fCard
>       fSuit  = suit fCard
>       tCards = fst $ takeCards 1 to b
>       tRank  = rank $ head tCards
>       tSuit  = suit $ head tCards
>   in case parsePile to of
>        Talon -> (False, concat ["can not move cards to ",
>                                 show to, "."])
>        Tableau ->
>          case tCards of
>            []     -> if fRank == King then (True, "")
>                      else (False, kingErr to)
>            (c:cs) -> if fRank /= King && succ fRank == tRank then
>                        (True,  "")
>                      else (False, rankErr fRank tRank)
>        Foundation ->
>          case tCards of
>            []     -> if fRank == Ace then (True,  "")
>                      else (False, aceErr to)
>            (c:cs) -> if tRank /= King && succ tRank == fRank &&
>                         fSuit == tSuit then
>                        (True,  "")
>                      else (False, suitRankErr fCard c)


> parseMove :: String -> String -> String -> (Move, String)
> parseMove n from to = let n' = readMaybe n :: Maybe Int in
>   case n' of
>     -- Move max number of cards
>     Nothing    -> parseMove (show maxn) from to
>     Just count ->
>       if count <= 0 || count > 12 then
>         (InvalidMove,
>          "must move at least 1 card and no more than 12.")
>       else
>         let from' = readMaybe from :: Maybe Pile
>         in case from' of
>              Nothing -> (InvalidMove, "invalid \"from\" parameter")
>              Just f  ->
>                let to' = readMaybe to :: Maybe Pile
>                in case to' of
>                     Nothing -> (InvalidMove,
>                                 "invalid \"to\" parameter")
>                     Just t  -> if f == t then
>                                  (InvalidMove,
>                                   "can't move to the same column")
>                                else (Move count f t, "")

> parsePile  :: Pile -> PileType
> parsePile m = if m == W      then Talon
>   else if m >= T1 && m <= T7 then Tableau
>   else                            Foundation

> takeCards :: Int -> Pile -> Board -> ([Card], Board)
> takeCards n from b = let idx = fromEnum from in
>   case parsePile from of
>     Talon -> -- Can only ever take 1 from Talon, ignore n
>       case talon b of
>         []     -> ([], b)
>         (c:cs) -> ([c], b { talon = cs })
>     Tableau ->
>       case tableau b of
>         [] -> ([], b)
>         cs -> let idx' = idx - 1
>                   -- Loop through the tableau piles until we find
>                   -- our target index. Drop the taken cards, and
>                   -- reveal the top card (if it's not already)
>                   newTab = map flipTableau
>                     [if i == idx' then dropFilter $ cs!!i
>                      else cs!!i | i <- [0..fromEnum T7-1]]
>           in (takeFilter $ cs!!idx', b { tableau = newTab })
>     Foundation ->
>       case (foundations b) of
>         [] -> ([], b)
>         cs -> let idx' = idx - 8
>                   -- Loop through the foundation piles until we find
>                   -- our target index. Drop the taken cards
>                   newFoun = [if i == idx' then dropFilter $ cs!!i
>                              else cs!!i | i <- [0..fromEnum F4-8]]
>           in (takeFilter $ cs!!idx', b { foundations = newFoun })
>   where takeFilter = if n == maxn then takeWhile (faceUp) else take n
>         dropFilter = if n == maxn then dropWhile (faceUp) else drop n

> addCards :: [Card] -> Pile -> Board -> Board
> addCards cards to b = let idx = fromEnum to
>   -- Loop through the tableau piles until we find our target
>   -- index. Add cards to the head.
>   in case parsePile to of
>        Talon   -> setErr b ("can't add cards to Talon")
>        Tableau -> let ts = tableau b
>          in b { message = "",
>                 tableau = [if i == idx-1 then cards ++ ts!!i
>                            else ts!!i | i <- [0..fromEnum T7-1]] }
>        Foundation -> let fs = foundations b
>          in b { message = "",
>                 foundations = [if i == idx-8 then (reverse cards)++fs!!i
>                                else fs!!i | i <- [0..fromEnum F4-8]] }

Testing

> newTestBoard :: Board
> newTestBoard =
>   let deck        = newDeck
>       tableau     = dealTableau deck
>       foundations = emptyFoundationList
>       -- Put the remaining items not dealt into the stock
>       stock       = drop (sum $ map length tableau) deck
>       talon       = emptyDeck
>       message     = "Let's play a game of Solitaire!\n" ++
>                     "Type 'h' for help."
>   in Board tableau foundations stock talon message False

> allDiff       :: (Eq a) => [a] -> Bool
> allDiff []     = True
> allDiff (x:xs) = x `notElem` xs && allDiff xs

> newBoardTest :: Bool
> newBoardTest  =
>   let board = newTestBoard
>       stock_len = length $ stock board
>       talon_len = length $ talon board
>       tab_len   = sum $ map length $ tableau board
>       found_len = sum $ map length $ foundations board
>   in stock_len == 24 && talon_len == 0 &&
>      tab_len == 28 && found_len == 0 &&
>      allDiff ((stock board) : (tableau board))

> flipTableauTest :: Bool
> flipTableauTest  =
>   let deck = newDeck
>       deck' = flipTableau deck
>   in (not $ faceUp $ head deck) &&
>      (faceUp $ head deck')

> drawCardTest :: Bool
> drawCardTest  =
>   let board  = newTestBoard
>       stock_len = length $ stock board
>       talon_len = length $ talon board
>       board' = drawCard board -- Check that draw 1 card works
>       stock_len' = length $ stock board'
>       talon_len' = length $ talon board'
>       -- Check that drawing all cards results an erro
>       board'' = drawCard $ board' { stock = drop 23 (stock board') }
>   in
>      (stock_len - stock_len') == 1 &&
>      (talon_len' - talon_len) == 1 &&
>      (message board'') == "Error: Stock pile is empty!"

> autoMoveTest :: Bool
> autoMoveTest  =
>   let board = newTestBoard
>       found_len = sum $ map length $ foundations board
>       board' = autoMove board
>       stock_len' = length $ stock board'
>       talon_len' = length $ talon board'
>       tab_len'   = sum $ map length $ tableau board'
>       found_len' = sum $ map length $ foundations board'
>   in
>       stock_len' == 0 && talon_len' == 0 &&
>       tab_len' == 0 && found_len == 0 && found_len' == 52
