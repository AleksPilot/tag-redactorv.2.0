module Main where

import System.IO as S
import System.Environment
import Data.List as List
import Data.Char
import Data.ByteString.Char8 as C8
import Data.ByteString.Internal as BI
import qualified Data.ByteString as BL
import GHC.Word 
import Auxiliary


main :: IO ()
main = do
   args <- getArgs
   let inputfile = List.head args
   let outputfile = args !! 1
   let typeOfTag = args !! 2
   let cont = List.drop 2 args
   let containts = spliter (List.drop 3 args)
   --S.putStr (List.unlines cont)
   --let splittedcontents = containts 
 --пофиксить handle, make stack version, list of tags
   src <- BL.readFile inputfile
   let n = BL.length src
   let pointToTags = n - 128
   --print pointToTags
  -- BL.writeFile outputfile (simpleRewriter src (pointToTags+certainTagPointer typeOfTag) containts (maxSized typeOfTag))

   BL.writeFile outputfile (List.foldl oneArgumentApply src (List.head (newtagParser containts))) -- wtf со свертками
   -- List.foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b, 
   -- oneArgumentApply :: BL.ByteString -> String -> BL.ByteString 
   -- src :: BL.ByteString 
   -- newtagParser :: String -> [String], newtagParser containts :: [String]
   -- почему каждая строка не применяется при помощи oneArgumentApply к src? 
  
