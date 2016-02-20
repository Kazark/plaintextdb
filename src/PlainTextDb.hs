{-# LANGUAGE RecordWildCards #-}
module PlainTextDb where

import Data.Char
import Data.List

data Token =
    VertSep -- |
    | Space
    | Underline -- _
    | BottomLine -- `
    | Contents String
    deriving (Eq, Show)

data CellStyle = Plain | Underlined deriving (Eq, Show)

data Cell = Cell {
      style :: CellStyle
    , contents :: String
    , width :: Int -- Actual formatted width: >= contents + 2
} deriving (Eq, Show)

data TextRow = TextRow {
      indent :: Int
    , cells :: [Cell]
} deriving (Eq, Show)

type TextTable = [TextRow]

rowWidth :: TextRow -> Int
rowWidth TextRow { indent = indent, cells = cells } =
    indent + sum (map width cells)

data StructureError = UnequalRows deriving (Eq, Show)

validateWidth :: TextTable -> Bool
validateWidth = allEq . map rowWidth where
    allEq [] = True
    allEq (x:xs) = all (== x) xs

validate :: TextTable -> Maybe StructureError
validate txttbl =
    if validateWidth txttbl
    then Nothing
    else Just UnequalRows

formatCell :: Cell -> String
formatCell Cell { .. } =
    let spaceChar =
            case style of
                Plain -> ' '
                Underlined -> '_'
    in spaceChar : contents ++ replicate (width - length contents - 1) spaceChar

formatRow :: TextRow -> String
formatRow TextRow { indent = _, cells = cells } = 
    "|" ++ intercalate "|" (map formatCell cells) ++ "|"

format :: TextTable -> [String]
format = map (const "")

