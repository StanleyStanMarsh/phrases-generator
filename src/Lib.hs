module Lib
    ( someFunc
    ) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T

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

