{-# LANGUAGE OverloadedStrings #-}

-- This module defines the board. A board is an array of CellType elements indexed by a tuple of ints: the height and width.
--
-- for example, The following array represents a 3 by 4 board (left top corner is (1,1); right bottom corner is (3,4)) with a snake at
-- (2, 2) and (2, 3) and an apple at (3,4)
--
-- < ((1,1) : Empty), ((1,2) : Empty), ((1,3) : Empty),     ((1,2) : Empty)
-- , ((2,1) : Empty), ((2,2) : Snake)  ((2,3) : SnakeHead)  ((2,4) : Empty)
-- , ((3,1) : Empty), ((3,2) : Empty), ((3,3) : Empty),     ((3,4) : Apple) >
--
-- Which would look like this:
--
-- - - - -
-- - 0 $ -
-- - - - X
module RenderState where

-- This are all imports you need. Feel free to import more things.

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks)
import Control.Monad.Trans.State.Strict (State, evalState, get, gets, modify, put, runState)
import Data.Array (Array, listArray, (!), (//))
import qualified Data.ByteString.Builder as B

type RenderStep a = ReaderT BoardInfo (State RenderState) a

-- A point is just a tuple of integers.
type Point = (Int, Int)

-- | Cell types. We distinguish between Snake and SnakeHead
data CellType = Empty | Snake | SnakeHead | Apple deriving (Show, Eq)

-- | The board info is just a description of height and width.
data BoardInfo = BoardInfo {height :: Int, width :: Int} deriving (Show, Eq)

type Board =
  -- | The board is an Array indexed by points with elements of type CellType
  Array Point CellType

-- | A delta is a small change in the board at some points. For example [((2,2), SnakeHead), ((2,1), Empty)]
--   would represent the change "cell (2,2) should change to become the SnakeHead and cell (2,1) should change by an empty cell"
type DeltaBoard = [(Point, CellType)]

-- | The render message represent all message the GameState can send to the RenderState
--   Right now Possible messages are a RenderBoard with a payload indicating which cells change
--   or a GameOver message.
data RenderMessage = RenderBoard DeltaBoard | IncrementScore Int | GameOver deriving (Show, Eq)

-- | The RenderState contains the board and if the game is over or not.
data RenderState = RenderState {board :: Board, score :: Int, gameOver :: Bool} deriving (Show)

-- | Given The board info, this function should return a board with all Empty cells
emptyGrid :: BoardInfo -> Board
emptyGrid (BoardInfo h w) = listArray ((1, 1), (h, w)) [Empty | _ <- [1 .. h * w]]

{-
This is a test for emptyGrid. It should return
array ((1,1),(2,2)) [((1,1),Empty),((1,2),Empty),((2,1),Empty),((2,2),Empty)]
-}
-- >>> emptyGrid (BoardInfo 2 2)
-- (Array.!): undefined array element

-- | Given BoardInfo, initial point of snake and initial point of apple, builds a board
buildInitialBoard ::
  -- | Board size
  BoardInfo ->
  -- | initial point of the snake
  Point ->
  -- | initial Point of the apple
  Point ->
  RenderState
buildInitialBoard bi sPos applePos = RenderState grid 0 False
  where
    grid = emptyGrid bi // [(sPos, SnakeHead), (applePos, Apple)]

{-
This is a test for buildInitialBoard. It should return
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}
-}
-- >>> buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = False}

-- | Given tye current render state, and a message -> update the render state
updateRenderState :: RenderMessage -> RenderStep ()
updateRenderState rm = do
  rs <- lift get
  case rm of
    GameOver -> do
      lift $ modify (\rs_ -> rs_ {gameOver = True})
    RenderBoard db -> do
      lift $ modify (\rs_ -> rs_ {board = board rs // db})
    IncrementScore incr -> do
      scount <- lift $ gets score
      lift $ modify (\rs_ -> rs_ {score = incr + scount})

updateMessages :: [RenderMessage] -> RenderStep ()
updateMessages = mapM_ updateRenderState

{-
This is a test for updateRenderState

message1 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}

message2 should return:
RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}
-}
-- >>> initial_board =  buildInitialBoard (BoardInfo 2 2) (1,1) (2,2)
-- >>> message1 = RenderBoard [((1,2), SnakeHead), ((2,1), Apple), ((1,1), Empty)]
-- >>> message2 = GameOver
-- >>> updateRenderState initial_board message1
-- >>> updateRenderState initial_board message2
-- RenderState {board = array ((1,1),(2,2)) [((1,1),Empty),((1,2),SnakeHead),((2,1),Apple),((2,2),Apple)], gameOver = False}
-- RenderState {board = array ((1,1),(2,2)) [((1,1),SnakeHead),((1,2),Empty),((2,1),Empty),((2,2),Apple)], gameOver = True}

-- | Provisional Pretty printer
--   For each cell type choose a string to representing.
--   a good option is
--     Empty -> "- "
--     Snake -> "0 "
--     SnakeHead -> "$ "
--     Apple -> "X "
--   In other to avoid shrinking, I'd recommend to use some charachter followed by an space.
ppCell :: CellType -> B.Builder
ppCell Empty = "_ "
ppCell Snake = "0 "
ppCell SnakeHead = "$ "
ppCell Apple = "X "

ppScore :: Int -> B.Builder
ppScore s = "-----------\n" <> mainLine <> "\n" <> "-----------\n"
  where
    mainLine = "score: " <> B.intDec s

ppSpeed :: Int -> B.Builder
ppSpeed s = "-----------\n" <> mainLine <> "\n" <> "-----------\n"
  where
    mainLine = "speed: " <> B.intDec s

-- | convert the RenderState in a String ready to be flushed into the console.
--   It should return the Board with a pretty look. If game over, return the empty board.
renderStep :: [RenderMessage] -> RenderStep B.Builder
renderStep rms = do
  updateMessages rms
  (BoardInfo h w) <- ask
  b <- lift $ gets board
  isGameOver <- lift $ gets gameOver
  currScore <- lift $ gets score
  let bl =
        [ ppCell (if isGameOver then Empty else b ! (x, y)) <> (if y == w then "\n" else "")
          | x <- [1 .. h],
            y <- [1 .. w]
        ]
  return $ ppScore currScore <> foldl1 (<>) bl <> "\n"

render :: [RenderMessage] -> BoardInfo -> RenderState -> (B.Builder, RenderState)
render rms = runState . runReaderT (renderStep rms)

{-
This is a test for render. It should return:
"- - - - \n- 0 $ - \n- - - X \n"

Notice, that this depends on what you've chosen for ppCell
-}
-- >>> board = listArray ((1,1), (3,4)) [Empty, Empty, Empty, Empty, Empty, Snake, SnakeHead, Empty, Empty, Empty, Empty, Apple]
-- >>> board_info = BoardInfo 3 4
-- >>> render_state = RenderState board  False
-- >>> render board_info render_state
-- "_ _ _ _ \n_ 0 $ _ \n_ _ _ A \n"
