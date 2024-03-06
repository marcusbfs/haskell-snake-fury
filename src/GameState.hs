{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

-- |
-- This module defines the logic of the game and the communication with the `Board.RenderState`
module GameState where

-- These are all the import. Feel free to use more if needed.

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (State, get, gets, modify, runState, StateT (runStateT), Functor, Monad)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import RenderState (BoardInfo (..), DeltaBoard, Point)
import qualified RenderState as Board
import System.Random (StdGen, uniformR)

type GameStep a = ReaderT BoardInfo (State GameState) a

-- newtype GameStep a = GameStep {runGameStep :: ReaderT BoardInfo (State GameState ) a} deriving (Functor, Applicative, Monad)

-- | The are two kind of events, a `ClockEvent`, representing movement which is not force by the user input, and `UserEvent` which is the opposite.
data Event = Tick | UserEvent Movement

-- The movement is one of this.
data Movement = North | South | East | West deriving (Show, Eq)

-- | The snakeSeq is a non-empty sequence. It is important to use precise types in Haskell
--   In first sight we'd define the snake as a sequence, but If you think carefully, an empty
--   sequence can't represent a valid Snake, therefore we must use a non empty one.
--   You should investigate about Seq type in haskell and we it is a good option for our porpouse.
data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

-- | The GameState represents all important bits in the game. The Snake, The apple, the current direction of movement and
--   a random seed to calculate the next random apple.
data GameState = GameState
  { snakeSeq :: SnakeSeq,
    applePosition :: Point,
    movement :: Movement,
    randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East

-- >>> opositeMovement North == South
-- >>> opositeMovement South == North
-- >>> opositeMovement East == West

-- | Purely creates a random point within the board limits
--   You should take a look to System.Random documentation.
--   Also, in the import list you have all relevant functions.
makeRandomPoint :: GameStep Point
makeRandomPoint = do
  rgen <- gets randomGen
  (BoardInfo maxh maxw) <- ask
  let (rh, rgen') = uniformR (1, maxh) rgen
  let (rw, rgen'') = uniformR (1, maxw) rgen'
  modify
    ( \gs_ ->
        gs_
          { randomGen = rgen''
          }
    )
  return (rh, rw)

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}

-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq -> Bool
inSnake p (SnakeSeq shead sbody) =
  p == shead || isJust (S.elemIndexL p sbody)

{-
This is a test for inSnake. It should return
True
True
False
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> inSnake (1,1) snake_seq
-- >>> inSnake (1,2) snake_seq
-- >>> inSnake (1,4) snake_seq

moveWithBound :: Int -> Int -> (Int, Int) -> Int
moveWithBound orig change (minb, maxb)
  | newpos < minb = maxb
  | newpos > maxb = minb
  | otherwise = newpos
  where
    newpos = orig + change

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: GameStep Point
nextHead = do
  gs <- get
  (BoardInfo h w) <- ask
  let (shx, shy) = snakeHead $ snakeSeq gs
  return $ case movement gs of
    North -> (moveWithBound shx (-1) (1, h), shy)
    South -> (moveWithBound shx 1 (1, h), shy)
    West -> (shx, moveWithBound shy (-1) (1, w))
    East -> (shx, moveWithBound shy 1 (1, w))

{-
This is a test for nextHead. It should return
True
True
True
-}
-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,2)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> nextHead board_info game_state1 == (1,4)
-- >>> nextHead board_info game_state2 == (2,1)
-- >>> nextHead board_info game_state3 == (4,1)

-- | Calculates a new random apple, avoiding creating the apple in the same place,
-- | or in the snake body
newApple :: GameStep Point
newApple = loop
  where
    loop :: GameStep Point
    loop = do
      gs <- get
      randPoint <- makeRandomPoint
      if randPoint `inSnake` snakeSeq gs || randPoint == applePosition gs
        then loop
        else return randPoint

{- We can't test this function because it depends on makeRandomPoint -}

extendSnake :: Point -> GameStep RenderState.DeltaBoard
extendSnake newHeadPos = do
  gs <- get
  let ssnake = snakeSeq gs
  let shead = snakeHead ssnake
  let sbody = snakeBody ssnake
  let newSBody = shead S.<| sbody
  newApplePos <- newApple
  let renderMessage =
        [ (newApplePos, Board.Apple),
          (shead, Board.Snake),
          (newHeadPos, Board.SnakeHead)
        ]
  modify
    ( \gs_ ->
        gs_
          { snakeSeq = SnakeSeq newHeadPos newSBody,
            applePosition = newApplePos
          }
    )
  return renderMessage

displaceSnake :: Point -> GameStep RenderState.DeltaBoard
displaceSnake newHeadPos = do
  gs <- get
  let ssnake = snakeSeq gs
  let shead = snakeHead ssnake
  let sbody = snakeBody ssnake
  let newSBody =
        S.insertAt 0 shead (S.deleteAt (length sbody - 1) sbody)
  let lastSnakePosition =
        if S.null sbody
          then Nothing
          else Just [(sbody `S.index` (length sbody - 1), Board.Empty)]
  let renderMessage =
        [ (shead, Board.Snake),
          (newHeadPos, Board.SnakeHead)
        ]
          ++ fromMaybe [] lastSnakePosition
  modify
    ( \gs_ ->
        gs_ {snakeSeq = SnakeSeq newHeadPos newSBody}
    )
  return renderMessage

-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]
step :: GameStep [Board.RenderMessage]
step = do
  newHeadPos <- nextHead
  gs <- get
  let ateApple = newHeadPos == applePosition gs
  let ateItself = newHeadPos `inSnake` snakeSeq gs

  -- What can happen:
  -- The snake eats the apple: move to the directin of the apple and gain a new body part
  -- The snake eats itself: game over!
  -- The snake just moves to a position.

  if ateApple
    then do
      delta <- extendSnake newHeadPos
      return [Board.RenderBoard delta, Board.IncrementScore 1]
    else
      if ateItself
        then do return [Board.GameOver]
        else do
          delta <- displaceSnake newHeadPos
          return [Board.RenderBoard delta]

move :: Event -> BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
move event binf gstate =
  let (delta, gstate') =
        case event of
          Tick -> move' binf gstate
          UserEvent m ->
            if movement gstate == opositeMovement m
              then move' binf gstate
              else move' binf $ gstate {movement = m}
   in (delta, gstate')
  where
    move' = runState . runReaderT step
    -- move' :: BoardInfo -> GameState -> ([Board.RenderMessage], GameState)
    -- move' b g =  step  b g

{- This is a test for move. It should return

RenderBoard [((1,4),SnakeHead),((1,1),Snake),((1,3),Empty)]
RenderBoard [((2,1),SnakeHead),((1,1),Snake),((3,1),Apple)] ** your Apple might be different from mine
RenderBoard [((4,1),SnakeHead),((1,1),Snake),((1,3),Empty)]

-}

-- >>> snake_seq = SnakeSeq (1,1) (Data.Sequence.fromList [(1,2), (1,3)])
-- >>> apple_pos = (2,1)
-- >>> board_info = BoardInfo 4 4
-- >>> game_state1 = GameState snake_seq apple_pos West (System.Random.mkStdGen 1)
-- >>> game_state2 = GameState snake_seq apple_pos South (System.Random.mkStdGen 1)
-- >>> game_state3 = GameState snake_seq apple_pos North (System.Random.mkStdGen 1)
-- >>> fst $ move board_info game_state1
-- >>> fst $ move board_info game_state2
-- >>> fst $ move board_info game_state3
-- RenderBoard [((1,3),Empty),((1,1),Snake),((1,4),SnakeHead)]
-- RenderBoard [((1,3),Empty),((1,1),Snake),((4,1),SnakeHead)]
-- RenderBoard [((4,3),Apple),((1,1),Snake),((2,1),SnakeHead)]
