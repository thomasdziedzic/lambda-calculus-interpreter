module Lib
{-
    ( Expression(..)
    , parser
    , myParse
    , reduce
    , converge
    , prettify
    , execute
    ) where
    -}
where

-- TODO switch to megaparsec
-- https://www.stackage.org/lts-7.16/package/megaparsec-5.0.1

import Text.Parsec (between, char, oneOf, lower, try, many1, parse, chainl1, space, eof)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>), (<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Expression
    = Variable String
    | Abstraction String Expression
    | Application Expression Expression
    deriving (Eq, Show)

parser :: Parser Expression
parser = abstraction <|> application <|> parenthesis <|> variable
  where
    parenthesis = between (char '(') (char ')') (parser)
    variable = Variable <$> many1 (oneOf ['a' .. 'z'])
    abstraction = do
        char 'λ'
        param <- many1 (oneOf ['a' .. 'z'])
        char '.'
        body <- parser
        return $ Abstraction param body
    application = (parenthesis <|> variable <|> abstraction) `chainl1` (space >> return Application)

myParse :: String -> Either String Expression
myParse s =
    case parse parser "(source)" s of
        Left err -> Left $ show err
        Right s -> Right s

reduce :: Expression -> Expression
reduce (Application e1@(Variable _) e2) = Application e1 (reduce e2)
reduce (Abstraction x (Application e1@(Variable v1) (Variable v2)))
    | x == v2 && x /= v1 = e1
reduce (Application e1@(Abstraction param body) e2) = {-alphaConversion e1 e2 -} substitute body param e2
reduce (Application e1@(Application _ _) e2) = reduce (Application (reduce e1) e2)
reduce (Abstraction p b) = Abstraction p (reduce b)
reduce e@(Variable _) = e

substitute :: Expression -> String -> Expression -> Expression
substitute e@(Abstraction p b) v e'
    | p == v = e -- do not replace variables with the same name that show up in a tighter scope
    | otherwise = Abstraction p (substitute b v e')
substitute e@(Variable s) v e'
    | s == v = e'
    | otherwise = e
substitute (Application e1 e2) v e' = Application (substitute e1 v e') (substitute e2 v e')

candidates :: Set String
candidates = Set.fromList $ map (\x -> [x]) $ ['a' .. 'z'] ++ ['A' .. 'Z']

boundVariables :: Expression -> Set String
boundVariables e = fst $ allVariables e Set.empty Set.empty

freeVariables :: Expression -> Set String
freeVariables e = snd $ allVariables e Set.empty Set.empty

allVariables :: Expression -> Set String -> Set String -> (Set String, Set String)
allVariables (Variable x) bound free
    | x `Set.member` bound = (bound, free)
    | otherwise = (bound, Set.insert x free)
allVariables (Abstraction p b) bound free = allVariables b (Set.insert p bound) free
allVariables (Application e1 e2) bound free = (b1 `Set.union` b2, f1 `Set.union` f2)
  where
    (b1, f1) = allVariables e1 bound free
    (b2, f2) = allVariables e2 bound free

rename :: Map String String -> Expression -> Set String -> Expression
rename m (Abstraction p b) boundVariables = Abstraction (Map.findWithDefault p p m) (rename m b boundVariables')
  where
    boundVariables' = Set.insert p boundVariables
rename m e@(Variable v) boundVariables
    | v `Set.member` boundVariables = Variable (Map.findWithDefault v v m)
    | otherwise = e
rename m (Application e1 e2) boundVariables = Application (rename m e1 boundVariables) (rename m e2 boundVariables)

alphaConversion :: Expression -> Either String Expression
alphaConversion (Application (Abstraction p e1) e2)
    | Set.size renamePool < Set.size boundE1 = Left "not enough variables to do an alpha rename"
    | otherwise = Right $ Application (Abstraction (Map.findWithDefault p p renameMap) (rename renameMap e1 Set.empty)) e2
    where
        boundE1 = boundVariables e1
        renamePool = candidates Set.\\ (freeVariables e2)
        renameMap = Map.fromList $ zip (Set.toList boundE1) (Set.toList renamePool)
alphaConversion e = Right e

converge :: Eq a => (a -> a) -> a -> a
converge f x
    | x == x' = x
    | otherwise = converge f x'
    where
        x' = f x

prettify :: Expression -> String
prettify (Variable v) = v
prettify (Application e1 e2@(Application e21 e22)) = (prettify e1) ++ " (" ++ (prettify e2) ++ ")"
prettify (Application e1 e2) = (prettify e1) ++ " " ++ (prettify e2)
prettify (Abstraction p b) = "λ" ++ p ++ "." ++ (prettify b)

convergeReduce :: Expression -> Either String Expression
convergeReduce e = do
    e' <- alphaConversion e
    let e'' = reduce e'
    if e == e''
        then return e
        else convergeReduce e''

execute :: String -> String
execute s =
    case prettify <$> (myParse s >>= convergeReduce) of
        Left err -> show err
        Right s -> s
