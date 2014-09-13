-- My game plan is: against bots who try to simulate me, don't try to be clever,
-- just play titForTat. Against bots who don't try to simulate me, I can predict
-- their moves, so exploit them.

--   Try not to take too long when simulating: bots who try to simulate me but
-- time out, might decide to defect against me on principle. Currently failing
-- at this.

--   Naturally, how well I do depends on the other bots in play. JusticeBot
-- fucks me over: we fall into (D,D) when other bots get (C,C) against ver
-- (potentially even (D,C), but I'm not sure what that bot would be doing). I'm
-- hoping JusticeBot gets eliminated early on.

module DMRB where

import Bots
import Tournament

import Control.Monad (replicateM)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- Wrap an 'IO' computation so that it prints execution time in
-- microseconds. System.TimeIt provides this, but uses seconds. This isn't used
-- in my bot at all, I just want it for testing.

timeIt :: IO a -> IO a
timeIt ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    printf "CPU time: %d\n" $ (t2-t1) `div` (10^6)
    return a

-- Simulate my opponent playing a round against me, and do the opposite of
-- whatever my opponent does. Limit my opponent to 10ms, cooperate if they go
-- over. Someone on LW wrote this.
trollBot :: Bot
trollBot = Bot run where
  run op hist = do
    simulation <- time 10000 . runBot op trollBot $ invert hist
    return (case simulation of
              Nothing -> Cooperate
              Just Cooperate -> Defect
              Just Defect -> Cooperate)

-- Defect against bots that are trying to simulate me. Cooperate against
-- everyone else.
uncooperativeBot :: Bot
uncooperativeBot = Bot run where
  run op hist = do
    clever <- isBotClever op
    case clever of
      True -> return Defect
      False -> return Cooperate

-- Against a dumb opponent, work out how I can get the most amount of points.
davidMonRoBot :: Bot
davidMonRoBot = Bot run where
  run op hist = do
    itsMoves <- replicateM 9 $ dumbBotsNextMove op
    movesAndScores <- mapM (\m -> exploit 5 op hist m) itsMoves
    return $ fst $ head movesAndScores


-- Int - number of rounds forward to predict
-- Bot - opponent to try to exploit.
-- [Moves] - history to start with.
-- Maybe Choice - Just (the move it's going to do next round), or Nothing.
-- m (Choice, Int) - my best move, and how many points I expect from it.
exploit :: BotEnvironment m =>
    Int -> Bot -> [Moves] -> Maybe Choice -> m (Choice, Int)

-- If I can't work out what it's going to do, run titForTat. If we mix calls
-- with Nothing to calls with (Just m), then the Nothing calls will have
-- slightly lower scores, on average. But we shouldn't be mixing those,
-- hopefully.
exploit n bot hist Nothing
  = do
    choice <- runBot titForTatBot bot hist
    return (choice, snd $ totalScores hist)

exploit 0 bot hist (Just itsMove)
  = return (Defect, fst $ totalScores $ hist ++ [(Defect, itsMove)])

-- The normal case: work out what it's going to do next round for each of my
-- moves this round, and recurse to find out which one is better for me.
exploit rounds bot hist (Just itsMove)
  = do
    itsNextMoveCo <- runBot bot cooperateBot $ invert $ withMove Cooperate
    itsNextMoveDe <- runBot bot cooperateBot $ invert $ withMove Defect

    (_, scoreCo) <- recurse Cooperate itsNextMoveCo
    (_, scoreDe) <- recurse Defect itsNextMoveDe

    if scoreCo > scoreDe
      then return (Cooperate, scoreCo)
      else return (Defect, scoreDe)

  where withMove m = hist ++ [(m, itsMove)]
        recurse me you = exploit (rounds - 1) bot (withMove me) (Just you)


avgScores :: (Integral a, Fractional b) => [(Choice, a)] -> (b, b)
avgScores xs = let ((c,d), (nc,nd)) = agg' (0,0) (0, 0) xs
                in ((fromIntegral c)/(fromIntegral nc),
                    (fromIntegral d)/(fromIntegral nd))
  where agg' (c,d) (nc,nd) [] = ((c,d), (nc,nd))
        agg' (c,d) (nc,nd) ((Cooperate, s) : xs) = agg' (c+s, d) (nc+1, nd) xs
        agg' (c,d) (nc,nd) ((Defect, s) : xs) = agg' (c, d+s) (nc, nd+1) xs

-- against randombot, simulating more levels deep makes the results less useful.

-- Run forever.
timeoutBot :: Bot
timeoutBot = Bot run where
  run op hist = do
    infiniteLoop
    return Cooperate
  infiniteLoop = infiniteLoop

dumbBotsNextMove :: BotEnvironment m => Bot -> m (Maybe Choice)
dumbBotsNextMove bot = time 500 $ runBot bot timeoutBot []

isBotClever :: BotEnvironment m => Bot -> m Bool
isBotClever bot = do
  sim <- time 500 $ runBot bot timeoutBot []
  case sim of
    Nothing -> return True
    Just _ -> return False
