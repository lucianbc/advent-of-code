{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# HLINT ignore "Use replicate" #-}
{-# HLINT ignore "Avoid lambda" #-}

import Debug.Trace
import System.Environment

main = do
  lines <- readInput
  part <- readPart
  let result = case part of
        "part1" -> part1 lines
        "part2" -> part2 lines
        _ -> "Bad part argument. Must be \"part1\" or \"part2\""
  putStrLn $ "Result is \n" ++ result

patternsPart2 =
  [ [ "M.S",
      ".A.",
      "M.S"
    ],
    [ "S.M",
      ".A.",
      "S.M"
    ],
    [ "M.M",
      ".A.",
      "S.S"
    ],
    [ "S.S",
      ".A.",
      "M.M"
    ]
  ]

patternsPart1 =
  [ ["XMAS"],
    ["SAMX"],
    [ ['X'],
      ['M'],
      ['A'],
      ['S']
    ],
    [ ['S'],
      ['A'],
      ['M'],
      ['X']
    ],
    [ "X...",
      ".M..",
      "..A.",
      "...S"
    ],
    [ "...X",
      "..M.",
      ".A..",
      "S..."
    ],
    [ "...S",
      "..A.",
      ".M..",
      "X..."
    ],
    [ "S...",
      ".A..",
      "..M.",
      "...X"
    ]
  ]

printToMap :: [String] -> ((Int, Int), [String]) -> [String]
printToMap map ((line, col), pat) = do
  lineIndex <- [0 .. length map - 1]
  let mapLine = map !! lineIndex
  if lineIndex < line || lineIndex >= (line + length pat)
    then return mapLine
    else return $ do
      colIndex <- [0 .. length mapLine - 1]
      let mapChar = mapLine !! colIndex
      let patLine = pat !! (lineIndex - line)
      let patChar = patLine !! (colIndex - col)
      if colIndex < col || colIndex >= (col + length patLine)
        then return mapChar
        else return $ if patChar == '.' then mapChar else patChar

part1 :: [String] -> String
part1 = countPatterns patternsPart1

part2 :: [String] -> String
part2 = countPatterns patternsPart2

countPatterns :: [[String]] -> [String] -> String
countPatterns patterns lines =
  let template = take (length lines) $ repeat (take (length . head $ lines) (repeat '.'))
      matchedPatterns = do
        pat <- patterns
        ((line, col), tile) <- generateTiles pat lines
        let matches = matchesPattern pat tile
        if matches then [((line, col), pat)] else []
      wordsHighlight = foldr (\crt acc -> printToMap acc crt) template matchedPatterns
      renderHighlight = do
        wLine <- wordsHighlight
        wLine ++ "\n"
   in renderHighlight ++ "\n" ++ (show . length $ matchedPatterns)

matchesPattern :: [[Char]] -> [[Char]] -> Bool
matchesPattern pat input =
  let pairs = concat $ zipWith zip pat input
      allOk = map (\(patChr, inputChr) -> if patChr == '.' then True else patChr == inputChr) pairs
   in and allOk

generateTiles :: [[Char]] -> [[Char]] -> [((Int, Int), [[Char]])]
generateTiles reference map =
  let height = length reference
      width = length . head $ reference
      mapHeight = length map
      mapWidth = length . head $ map
      ys = [0 .. (mapHeight - height)]
      xs = [0 .. (mapWidth - width)]
   in do
        y <- ys
        x <- xs
        return ((y, x), crop2d y x height width map)

crop :: Int -> Int -> [a] -> [a]
crop start len input = take len $ drop start input

crop2d :: Int -> Int -> Int -> Int -> [[a]] -> [[a]]
crop2d y x height width input =
  let lines = crop y height input
   in map (crop x width) lines

readPart :: IO String
readPart = do
  args <- getArgs
  return $ args !! 1

readInput :: IO [String]
readInput = do
  args <- getArgs
  let filePath = head args
  inputContent <- readFile filePath
  return $ lines inputContent
