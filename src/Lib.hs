module Lib
    ( allSentences
    , runParser
    ) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, isSpace)
import Data.List (sortBy, groupBy, nub)

-- Parser
-- instances

newtype Parser a = Parser { runParser :: Text -> Maybe (Text, a) }

instance Functor Parser where
    -- хотим применить func над результатом парсера p
    fmap func (Parser p) = Parser f where
        -- парсер f возвращает:
        f origText = case p origText of
            Nothing -> Nothing -- Nothing, если парсер p возвращает Nothing
            Just (remainingP, resP) -> Just (remainingP, func resP) -- (остаток, resP обработанный функцией func), если p возвращает (остаток, resP)

instance Applicative Parser where
    -- возвращаем всегда (изначальная строка, передаваемое значение)
    pure text = Parser (\orig -> Just(orig, text))

    -- хотим чтобы был парсер, который применяет к остатку 1 парсера 2 парсер,
    -- а затем применяет 1 парсер ко 2
    (Parser u) <*> (Parser v) = Parser f where
        f origText = case u origText of
            Nothing -> Nothing
            -- remainingU - остаток 1 парсера
            Just (remainingU, resU) -> case v remainingU of
                Nothing -> Nothing
                -- remainingV - итоговый остаток, resU применяем над resV
                Just (remainingV, resV) -> Just (remainingV, resU resV)

instance Alternative Parser where
    -- парсер всегда возвращающий Nothing
    empty = Parser $ \_ -> Nothing

    Parser u <|> Parser v = Parser f where
        -- пытаемся применить парсер u
        f origText = case u origText of
            -- если он вернул Nothing, то применям парсер v
            Nothing -> v origText
            -- если вернул какой то результат, то оставляем результат
            res -> res

-- functions

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` ['.', '!', '?', ';', ':', '(', ')']

joinWords :: [Text] -> Text
joinWords = T.intercalate (T.pack " ")

-- parsers

satisfy :: (Char -> Bool) -> Parser Char
satisfy pr = Parser f where
    -- берем первый символ
    f cs = case T.uncons cs of
        Nothing -> Nothing
        -- если первый элемент соответствует предикату pr
        Just (fstChar, remainingText)
            | pr fstChar -> Just (remainingText, fstChar) -- то возвращаем (остаток, подоходящий первый элемент)
            | otherwise -> Nothing
    f _ = Nothing

word :: Parser Text
word = Parser f where
    f input = case runParser (some (satisfy isLetter)) input of
        Nothing -> Nothing
        Just (remaining, chars) -> Just (remaining, T.pack chars)

oneSpace :: Parser Char
oneSpace = satisfy isSpace

spaces :: Parser Text
spaces = (T.cons) <$> oneSpace <*> spaces <|> pure T.empty

punctuation :: Parser Char
punctuation = satisfy isPunctuation

skipJunk :: Parser ()
skipJunk = Parser f where
    f input = Just (T.dropWhile (\c -> not (isLetter c || isPunctuation c)) input, ())

sentence :: Parser [Text]
sentence = (\words _ -> words) 
    <$> some (word <* skipJunk)
    <*> some punctuation
    <* skipJunk
    <|> empty

allSentences :: Parser [[Text]]
allSentences = some sentence

sentenceAsText :: Parser Text
sentenceAsText = (\words _ -> joinWords words)
    <$> some (word <* skipJunk)
    <*> some punctuation
    <* skipJunk
    <|> empty

allSentencesAsText :: Parser [Text]
allSentencesAsText = some sentenceAsText

-- N-gram

type NGramMap = [(Text, [Text])]

toBiGrams :: [Text] -> [(Text, Text)]
toBiGrams words = zip words (tail words)

triple :: [a] -> [(a, a, a)]
triple (x:y:z:rest) = (x,y,z) : triple (y:z:rest)
triple _ = []

toTriGrams :: [Text] -> [(Text, Text)]
toTriGrams words = 
    [(T.concat [w1, T.pack " ", w2], w3) | (w1,w2,w3) <- triple words]

groupPairs :: [(Text, Text)] -> NGramMap
groupPairs pairs = map (\group -> (fst $ head group, map snd group)) 
                  $ groupBy (\x y -> fst x == fst y) 
                  $ sortBy (\x y -> compare (fst x) (fst y)) pairs

makeNGrams :: [Text] -> [(Text, Text)]
makeNGrams words = 
    -- Create bi-grams (word -> next word)
    toBiGrams words ++
    -- Create tri-grams (two words -> next word)
    toTriGrams words
  where
    -- Creates pairs (word, next word)
    toBiGrams :: [Text] -> [(Text, Text)]
    toBiGrams ws = zip ws (tail ws)

    -- Creates pairs (two words, next word)
    toTriGrams :: [Text] -> [(Text, Text)]
    toTriGrams ws = 
        [(T.concat [w1, T.pack " ", w2], w3) | (w1,w2,w3) <- triple ws]
