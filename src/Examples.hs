module Examples
  ( goodPull
--   , badPull
  , foo
  , pullFmap
  )
where

import           PullPush

-- Pull

goodPull :: Pull Int
goodPull = Pull 10 foo

-- badPull :: Pull Int
-- badPull = Pull 42 foo

{-@ foo :: {i:Int | i < 40} -> Int @-}
foo :: Int -> Int
foo i = i

pullFmap :: Pull Int
pullFmap = fmap (+1) goodPull

