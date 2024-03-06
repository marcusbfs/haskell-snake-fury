{-# LANGUAGE NumericUnderscores #-}

module Main where

import Control.Concurrent
  ( forkIO,
    threadDelay,
  )
import Control.Monad (unless)
import qualified Data.ByteString.Builder as BS.Builder
import EventQueue
  ( EventQueue (initialSpeed),
    readEvent,
    setSpeed,
    writeUserInput,
  )
import GameState (Event (..), GameState (movement), move, opositeMovement)
import Initialization (gameInitialization)
import RenderState (BoardInfo, RenderState (gameOver), render, score, updateMessages)
import qualified System.Console.ANSI as A
import System.Environment (getArgs)
import System.IO (BufferMode (NoBuffering), hSetBinaryMode, hSetBuffering, hSetEcho, stdin, stdout)

-- The game loop is easy:
--   - wait some time
--   - read an Event from the queue
--   - Update the GameState
--   - Update the RenderState based on message delivered by GameState update
--   - Render into the console
gameloop :: BoardInfo -> GameState -> RenderState -> EventQueue -> IO ()
gameloop binf gstate rstate queue = do
  new_speed <- setSpeed (score rstate) queue
  threadDelay new_speed
  event <- readEvent queue
  let (delta, gstate') = move event binf gstate
  let (screen, rstate') = render delta binf rstate
  let isGameOver = gameOver rstate'
  putStr "\ESC[2J" -- This cleans the console screen
  BS.Builder.hPutBuilder stdout screen
  unless isGameOver $ gameloop binf gstate' rstate' queue

-- | main.
main :: IO ()
main = do
  A.hideCursor
  -- enable reading key strokes
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  hSetBuffering stdout NoBuffering
  hSetBinaryMode stdout True

  -- Game Initializacion
  [h, w, fps] <- fmap read <$> getArgs
  let timeSpeed = 1_000_000 `div` fps -- One second is 1_000_000 microseconds, which is the unit used by GHC internally.
  (binf, gameState, renderState, eventQueue) <- gameInitialization h w timeSpeed

  -- Game Loop. We run two different threads, one for the gameloop (main) and one for user inputs.
  _ <- forkIO $ writeUserInput eventQueue
  let initialState = gameState
  gameloop binf initialState renderState eventQueue

  A.showCursor
