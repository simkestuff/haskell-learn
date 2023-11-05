{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO 
import Data.Maybe
import System.Posix (BaudRate(B2400))
import GHC.Exts.Heap (GenClosure(bytes), StgInfoTable (entry))
import qualified Data.Text.Internal.Read as T

type Author = T.Text
type Title = T.Text

data Book = Book {
    author :: Author,
    title :: Title } deriving (Show)

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat [paragraphOpen, titleInTags, authorInTags, paragraphClose]
    where paragraphOpen = "<p>\n"
          paragraphClose = "</p>\n"
          titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
          authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [htmlOpen, booksInTags, htmlClose]
    where htmlOpen = mconcat ["<html>\n",
                              "<head>\n<title>books</title>\n",
                              "<meta charset='utf-8'/>\n", 
                              "</head>\n",
                              "<body>\n"]
          htmlClose = mconcat ["</body>\n", "</html>"]
          booksInTags = mconcat (map bookToHtml books)

book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race"
    ,author = "Ligotti, Thomas"

}
book2 :: Book
book2 = Book {
    title = "A Short History of Decay"
    ,author = "Cioran, Emil"

}
book3 :: Book
book3 = Book {
    title = "The Tears of Eros"
    ,author = "Bataille, Georges"
}

myBooks :: [Book]
myBooks = [book1, book2, book3]


-- main :: IO ()
-- main = do
--     TIO.writeFile "books.html" (booksToHtml myBooks)




type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

data FieldMetadata = FieldMetadata {
    tag :: T.Text,
    fieldLength :: Int,
    fieldStart :: Int
} deriving Show

leaderLength :: Int
leaderLength = 24 

dirEntryLength :: Int
dirEntryLength = 12

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: B.ByteString -> Int
getRecordLength = rawToInt . B.take 5

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest bytes = B.splitAt firstRecordLength bytes
    where firstRecordLength = getRecordLength bytes

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords bytes = if bytes == B.empty
                   then []
                   else next : allRecords rest 
        where (next, rest) = nextAndRest bytes

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress = rawToInt . B.take 5 . B.drop 12

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
   where afterLeader = B.drop leaderLength record
         directoryLength = getDirectoryLength (getLeader record)

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else B.take dirEntryLength directory : splitDirectory rest 
            where rest = B.drop dirEntryLength directory

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
    where (tagAndLength, address) = B.splitAt 7 entry 
          textTag = E.decodeUtf8 (B.take 3 tagAndLength)
          theLength = rawToInt (B.drop 3 tagAndLength)
          theStart = rawToInt address

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMetadata = E.decodeUtf8 byteValue
    where baseRecord = B.drop (getBaseAddress record) record
          baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord 
          byteValue = B.take (fieldLength fieldMetadata) baseAtEntry


fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'


lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if null fmd
                                  then Nothing
                                  else Just (head fmd)
    where lofmd = (getFieldMetadata . splitDirectory . getDirectory) record
          fmd = filter (\aField -> aTag == tag aField) lofmd

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetaData) aChar record = 
        if null results
        then Nothing
        else Just ((T.drop 1 . head) results)
    where aField = getTextField record fieldMetaData
          subfields = T.split (== fieldDelimiter) aField
          results = filter ((== aChar) . T.head) subfields


lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag aSubfield record = lookupSubfield fieldMetaData aSubfield record
      where fieldMetaData = lookupFieldMetadata aTag record


lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe T.Text
lookupAuthor = lookupValue authorTag authorSubfield


marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors 
    where records = allRecords marcStream
          titles = map lookupTitle records
          authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks [] = []
pairsToBooks pairs = map (\(t,a) -> Book (fromJust t) (fromJust a)) justPairs
    where justPairs = filter (\(t,a) -> isJust t && isJust a) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords recNumber = booksToHtml . pairsToBooks . take recNumber . marcToPairs

main :: IO ()
main = do 
    bytes <- B.readFile "sample.mrc" 
    let processed = processRecords 500 bytes
    TIO.writeFile "books.html" processed

