module Computer.Evaluation (evaluate) where

import Chess.Board
import Chess.Board
import Chess.Board (Board)

--Hello world!
--Should take in a board and score it using helpers, I would prefer that this is ALL that this
--module does, and I would like for it to only export a single function! that would be neat

evaluate :: Board -> Int
evaluate b = 0