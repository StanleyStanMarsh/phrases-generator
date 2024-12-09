module Lib
    ( allSentences
    , runParser
    , generatePhrase
    , processText
    , NGramMap
    , generateDialogue
    ) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, isSpace)
import Data.List (sortBy, groupBy, nub)
import System.Random (RandomGen, randomR, next, split)

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

isWordChar :: Char -> Bool
isWordChar c = isLetter c || c == '\''

word :: Parser Text
word = Parser f where
    f input = case runParser (some (satisfy isWordChar)) input of
        Nothing -> Nothing
        Just (remaining, chars) -> 
            -- проверяем что слово начинается или заканчивается апострофом
            let word = T.pack chars
                cleanWord = T.dropAround (== '\'') word
            in if T.null cleanWord 
                then Nothing  -- если слово состоит только из апострофов, то возвращаем Nothing
                else Just (remaining, word)

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

toBiGramsJoined :: [Text] -> [(Text, Text)]
toBiGramsJoined ws = 
    [(w1, T.concat [w2, T.pack " ", w3]) | (w1,w2,w3) <- triple ws]

triple :: [a] -> [(a, a, a)]
triple (x:y:z:rest) = (x,y,z) : triple (y:z:rest)
triple _ = []

toTriGrams :: [Text] -> [(Text, Text)]
toTriGrams ws = 
    [(T.concat [w1, T.pack " ", w2], w3) | (w1,w2,w3) <- triple ws]


groupPairs :: [(Text, Text)] -> NGramMap
groupPairs pairs = map (\group -> (fst $ head group, map snd group)) 
                  $ groupBy (\x y -> fst x == fst y) 
                  $ sortBy (\x y -> compare (fst x) (fst y)) pairs

makeNGrams :: [Text] -> [(Text, Text)]
makeNGrams words = 
    -- создаем биграммы вида (word -> next word)
    toBiGrams words ++
    -- создаем биграммы вида (word -> two next words joined)
    toBiGramsJoined words ++
    -- создаем триграммы вида (two words -> next word)
    toTriGrams words

processText :: Text -> NGramMap
processText text = case runParser allSentences text of
    Nothing -> []
    Just (_, sentences) -> let
        -- берем все слова из всех предложений
        allWords = nub $ concat sentences
        -- создаем n-граммы из всех предложений и удаляем дубликаты
        allNGrams = groupPairs 
                   $ nub -- удаляем дубликаты
                   $ concatMap makeNGrams sentences
        -- добавляем слова, которые не имеют следующих слов
        singleWords = map (\w -> (w, [])) 
                     $ filter (\w -> not $ any (\(prefix, _) -> prefix == w) allNGrams) 
                     $ allWords
        in sortBy (\x y -> compare (fst x) (fst y)) 
           $ allNGrams ++ singleWords

generatePhrase :: RandomGen g => g -> Text -> NGramMap -> Either Text [Text]
generatePhrase gen firstWord nGrams = 
    -- если первого слова нет в словаре, то возвращаем ошибку (Left ошибка, Right [слова для фразы])
    case lookup firstWord nGrams of
        Nothing -> Left $ T.concat [T.pack "Word '", firstWord, T.pack "' not found in the dictionary"]
        -- если первое слово есть в словаре, то начинаем генерировать фразу
        Just nextWords -> 
            let (targetLength, newGen) = randomR (2, 15) gen
            in Right $ generatePhraseHelper newGen [firstWord] firstWord nGrams targetLength

generatePhraseHelper :: RandomGen g => g -> [Text] -> Text -> NGramMap -> Int -> [Text]
generatePhraseHelper gen acc lastKey nGrams targetLength
    | length acc >= targetLength = take targetLength acc  -- достигли максимальной длины
    | null possibleNextWords = acc  -- нет больше слов
    | otherwise = 
        let (idx, newGen) = randomR (0, length possibleNextWords - 1) gen
            nextWord = possibleNextWords !! idx
            -- если два слова, то разбиваем на два
            nextWords = T.words nextWord
            -- и добавляем в список слов фразы
            newAcc = acc ++ nextWords
            -- используем следующее слово (или два слова) как ключ для следующего шага
            newKey = nextWord
        -- рекурсивно вызываем функцию для следующего шага
        in generatePhraseHelper newGen newAcc newKey nGrams targetLength
  where
    -- находим список слов по данному ключу (Just [Text]), если такого ключа в словаре нет, то Nothing
    possibleNextWords = maybe [] id $ lookup lastKey nGrams

-- Найти первое подходящее слово от конца фразы, которое есть в словаре
findLastValidWord :: [Text] -> NGramMap -> Maybe Text
findLastValidWord [] _ = Nothing
findLastValidWord (word:rest) nGrams = 
    case lookup word nGrams of
        Just _ -> Just word
        Nothing -> findLastValidWord rest nGrams

type DialogueResponse = Either Text [Text]  -- Left - ошибка, Right - фраза
type DialogueTurn = (Int, DialogueResponse)  -- (номер модели, ответ)

generateDialogue :: RandomGen g => 
                   g ->           
                   Text ->        
                   NGramMap ->    
                   NGramMap ->    
                   Int ->         
                   [DialogueTurn]  -- возвращает список (номер модели, ответ)
generateDialogue gen firstWord dict1 dict2 depth = 
    let firstResponse = generatePhrase gen firstWord dict1
        (_, newGen) = next gen
    in (1, firstResponse) : generateDialogueHelper newGen firstResponse 1 dict1 dict2 depth []

generateDialogueHelper :: RandomGen g => 
                         g -> 
                         DialogueResponse -> -- последний ответ
                         Int ->              -- номер модели
                         NGramMap ->         -- первый словарь
                         NGramMap ->         -- второй словарь
                         Int ->              -- остаток глубины
                         [DialogueTurn] ->   -- накопленный диалог
                         [DialogueTurn]      -- финальный диалог
generateDialogueHelper _ _ _ _ _ 0 acc = reverse acc
generateDialogueHelper gen lastResponse speaker dict1 dict2 depth acc =
    let 
        -- следующая модель
        nextSpeaker = if speaker == 1 then 2 else 1
        
        -- словарь для следующей модели
        currentDict = if nextSpeaker == 1 then dict1 else dict2
        
        -- последнее подходящее слово из предыдущего ответа
        lastWords = case lastResponse of
            Right phrase -> reverse phrase  -- если слово есть
            Left _ -> []                   -- если нет, то пустой список
        
        -- последнее подходящее слово из предыдущего ответа
        nextWord = findLastValidWord lastWords currentDict
        
        -- следующий ответ (даже если не нашли подходящего слова)
        nextResponse = case nextWord of
            Nothing -> Left (T.pack "No valid word found to continue dialogue")
            Just word -> 
                let (newGen1, _) = split gen
                in generatePhrase newGen1 word currentDict
        
        (_, newGen2) = split gen
    in generateDialogueHelper 
        newGen2 
        nextResponse
        nextSpeaker 
        dict1 
        dict2 
        (depth - 1) 
        ((nextSpeaker, nextResponse) : acc)