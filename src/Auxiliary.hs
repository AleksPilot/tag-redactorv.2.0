module Auxiliary
    ( spliter,
    zerosymbol, 
    simpleRewriter,
    maxSized,
    certainTagPointer,
    newTagParser,
    oneArgumentApply,
    applying
    ) where

import Data.List as List
import Data.Char
import Data.ByteString.Char8 as C8
import Data.ByteString.Internal as BI
import qualified Data.ByteString as BL
import GHC.Word 


spliter :: [String] -> String --helps using containts with spaces
spliter xs = helper xs "" where
   helper [] acc = acc
   helper (x:xs) acc = helper xs (acc ++ x ++ " ")


fromTo :: String -> Int -> Int -> String
fromTo xs lowerEdge higherEdge = List.drop lowerEdge (List.take higherEdge xs)

isPref :: String -> String -> Bool
isPref "" _ = True
isPref _ "" = False
isPref f@(s:ss) (q:qs) = if (s==q) then isPref ss qs else False 

strPos :: String -> String -> [Int]
strPos f q = List.filter (>= 0) $ List.map (\(s,p) -> if (isPref q s) then p else (-1)) plist 
             where n = List.length f
                   plist = List.zip (List.take n $ iterate (List.drop 1) f) [0..(n-1)] --поиск всех вхождений нью-тэг в контэинтс

--newTagParser :: String -> [Int] -> [String]
--newTagParser str xs = if List.tail xs /= [] then [fromTo str (List.head xs + 8) (List.head (List.tail xs) - 1)] ++ newTagParser (List.drop (List.head (List.tail xs) + 8) str) (List.tail xs) else [str]

newTagParser :: String -> [String]
newTagParser str = if List.tail xs /= [] then [fromTo str (List.head xs + 8) (List.head (List.tail xs) - 1)] ++ newTagParser (List.drop (List.head (List.tail xs) + 8) str) else [str] where xs = strPos "new tag" str

zerosymbol :: Word8 
zerosymbol = 0

simpleRewriter :: BL.ByteString -> Int -> String -> Int -> BL.ByteString
simpleRewriter xs 0 ys maxSize = if List.length ys < maxSize then BL.append (BL.append (pack ys) (BL.replicate (maxSize-List.length ys) zerosymbol)) (BL.drop maxSize xs) else BL.append (pack (List.take maxSize ys)) (BL.drop maxSize xs) 
simpleRewriter xs n ys maxSize = BL.append (BL.take n xs) (simpleRewriter (BL.drop n xs) 0 ys maxSize)

oneArgumentApply :: BL.ByteString -> String -> BL.ByteString --function that will be mapped
oneArgumentApply xs tags = simpleRewriter xs (certainTagPointer (List.head (List.words tags))) (List.last (List.words tags)) (maxSized (List.head (List.words tags)))

maxSized :: String -> Int
maxSized "title" = 30
maxSized "artist" = 30
maxSized "album" = 30
maxSized "year" = 4
maxSized "comment" = 28
maxSized "zerobyte" = 1
maxSized "track" = 1
maxSized "genre" = 1
maxSized _ = error "Input error, tag does not exist"

certainTagPointer :: String -> Int
certainTagPointer "title" = 3
certainTagPointer "artist" = 33
certainTagPointer "album" = 63
certainTagPointer "year" = 93
certainTagPointer "comment" = 97
certainTagPointer "zerobyte" = 125
certainTagPointer "track" = 126
certainTagPointer "genre" = 127
certainTagPointer _ = error "Input error, tag does not exist"

applying :: (BL.ByteString -> String -> BL.ByteString) -> BL.ByteString  -> [String] -> BL.ByteString 
applying oneArgApply source [] = source
applying oneArgApply source args = applying oneArgApply (oneArgApply source (List.head args)) (List.tail args)