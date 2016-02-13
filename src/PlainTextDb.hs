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
      indent :: Int
    , cells :: [Cell]
} deriving (Eq, Show)

type TextTable = [TextRow]

rowWidth :: TextRow -> Int
rowWidth TextRow { indent = indent, cells = cells } =
    indent + sum ( map width cells)

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
formatCell Cell { style = style, contents = _, width = _ } =
    case style of
        Plain -> " "
        Underlined -> "_"

formatRow :: TextRow -> String
formatRow _ = ""

format :: TextTable -> [String]
format = map (const "")

