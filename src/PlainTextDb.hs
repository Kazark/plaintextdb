module PlainTextDb where

import Data.Char

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
    , width :: Int
} deriving (Eq, Show)

data TextRow = TextRow {
      indentedNCells :: Int
    , cells :: [Cell]
} deriving (Eq, Show)

type TextTable = [TextRow]

data StructureError = UnequalRows deriving (Eq, Show)

rowWidth :: TextRow -> Int
rowWidth TextRow { indentedNCells = indent, cells = cells } =
    indent + sum ( map width cells)

validateWidth :: TextTable -> Bool
validateWidth = allEq . map rowWidth where
    allEq [] = True
    allEq (x:xs) = all (== x) xs

validate :: TextTable -> Maybe StructureError
validate txttbl =
    if validateWidth txttbl
    then Nothing
    else Just UnequalRows

format :: TextTable -> [String]
format = map (const "")

