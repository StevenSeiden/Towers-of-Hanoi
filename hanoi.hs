import Extras((<?>),(<$>),printedNumbers)
-- Towers of Hanoi

-------------------------------------------------------------------------------
-- Configuration: Number of pieces and moves
-------------------------------------------------------------------------------

-- solvePuzzle ------------------------------------
--              n       start   dest    others
solvePuzzle :: (Number, Number, Number, [Number]) -> [Move]

solvePuzzle(0, _, _, _) = []

solvePuzzle(1, start, final, _) = [move(start, final)]

solvePuzzle(n, _, _, []) = error "Impossible to solve :("

solvePuzzle(_, start, final, _) | start == final = []

solvePuzzle(numPieces, start, final, [single]) = 
    solvePuzzle(numPieces - 1, start, single, [final])
 ++ solvePuzzle(1, start, final, [])
 ++ solvePuzzle(numPieces - 1, single, final, [start])
 
solvePuzzle(numPieces, start, final, others) | numPieces - 1 <= length(others) =  -- TODO shorten list if n - 1 < len others
    ((\o -> move(start, o)) <$> trimmedOthers)
 ++ [move(start, final)]
 ++ ((\o -> move(o, final)) <$> reversed(trimmedOthers))
    where
      trimmedOthers =  first(others, numPieces - 1)
      
solvePuzzle(numPieces, start, final, [others1, others2]) = 
    solvePuzzle(partialPieces, start, others1, [final, others2])
  ++solvePuzzle(numPieces - partialPieces, start, final, [others2])
  ++solvePuzzle(partialPieces, others1, final, [start, others2])
    where
      partialPieces = optimal --truncation(numPieces - 3/4)
      optimal = 1+ numPieces-rounded(sqrt(2*numPieces+1))
      
      
solvePuzzle(numPieces, start, final, [others1, others2,other3]) = 
    solvePuzzle(partialPieces, start, others1, [final, others2, other3])
  ++solvePuzzle(numPieces - partialPieces, start, final, [others2,other3])
  ++solvePuzzle(partialPieces, others1, final, [start, others2, other3])
    where
      partialPieces = optimal --truncation(numPieces - 3/4)
      optimal = 1+ numPieces-rounded(sqrt(2*numPieces+1))
      
solvePuzzle(numPieces, start, final, others) = error message
  where
    message = joined([printed(numPieces), " ", 
      printed(start), " ", 
      printed(final), " ", 
      printedNumbers(others)])
 
---------------------------------------------------

moveSequence = solvePuzzle(numPieces,1,3,[2,4])


numPieces = 70 -- try these afterwards: 35, 70, 126

frameDelay = 0 -- seconds

-------------------------------------------------------------------------------
-- DO NOT MODIFY THE CODE BELOW THIS LINE
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Game logic
-------------------------------------------------------------------------------

main = interactionOf(start,step,input,draw)

type Move = [Stack] -> [Stack]
type Stack = [Number]

data Model = Model
  { total :: Number
  , timer :: Number
  , moves :: [Move]
  , stacks :: [Stack]
  }
  
start _ = Model
  { total = 0
  , timer = 0
  , moves = moveSequence
  , stacks = [[1..numPieces],[],[],[],[],[]]
  }

step (model@Model{..},dt) =
  if timer >= frameDelay
  then nextMove
  else model { timer = timer + dt }

  where
  nextMove =
    if empty(moves) then model
    else model { total = total + 1
               , timer = 0
               , moves = rest(moves,1)
               , stacks = (moves#1)(stacks)
               }
  
input (model,_) = model

-------------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------------

draw(Model{..}) = stackPics
            & translated(text(printed(total)),-8,9)
  where
  stackPics = pictures[translated(drawStack i s,x,y)
                      | i <- [1..]
                      | s <- stacks
                      | y <- [2.5,-7.5]
                      , x <- [-5,0,5]
                      ]
  
drawStack n stack =
  pictures[translated(piece i,0,(0.5+num-pos-1)*height)
          | i <- stack
          | pos <- [0..]
          ]
  & thickPolyline([(-2,-0.2),(2,-0.2)],0.2)
  & thickPolyline([(0,-0.2),(0,totalheight)],0.2)
  & translated(text("Stack " <> (printed n)),0,-1)
  & translated(dilated(text(printed(length(stack))),0.75),0,-1.75)
  where
  width = 3.5 / numPieces
  height = totalheight/(numPieces+1)
  totalheight = 7
  num = length(stack)
  piece i =
    colored(solidRectangle(width*i,height),assortedColors#i)
    & border
    where
    border | numPieces < 20 = thickRectangle(width*i,height,0.2)
           | otherwise = rectangle(width*i,height)

-------------------------------------------------------------------------------
-- Manipulation of the stacks
-------------------------------------------------------------------------------

getStack n stacks = stacks#n#1

popStack n stacks = [if i == n then rest(s,1) else s
                    | i <- [1..]
                    | s <- stacks
                    ]

pushStack n x stacks = [if i == n then x:s else s
                       | i <- [1..]
                       | s <- stacks
                       ]

emptyStack n stacks = empty (stacks#n)

move(i,j)(stacks) =
  if emptyStack i stacks then stacks
  else if emptyStack j stacks then moveit
  else if x < getStack j stacks then moveit
  else stacks
  where
  x = getStack i stacks
  moveit = popStack i (pushStack j x stacks)

