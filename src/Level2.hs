{-# OPTIONS_GHC -Wno-orphans #-}
-- | Домашка 6. Задачи к сессии. Ord дерева из уровня 1 — учебный orphan.
module Level2 where

import Data.Int (Int8)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Level1 (IntTree (..), StructEq (..))
import MetaUtils (todo, todoImpl)
import Text.Read (readMaybe)
import Data.List (stripPrefix)


-- 2.1. Порядок деревьев, согласованный с Eq из 1.2.
-- IntLeaf равен IntLeaf и меньше любого IntNode.
-- Узлы сравниваются лексикографически: левое поддерево, ВТОРАЯ компонента
-- пары, правое поддерево. Первая компонента пары по-прежнему игнорируется.
-- Выберите минимально необходимый набор методов. Законы Ord — лекция 6.

instance Ord IntTree where
  compare t0 t1 = case (t0, t1) of
    (IntLeaf, IntLeaf) -> EQ
    (IntLeaf, IntNode _ _ _) -> LT
    (IntNode _ _ _, IntLeaf) -> GT
    (IntNode a1 (_, b1) c1, IntNode a2 (_, b2) c2) ->
      a1 `compare` a2 <> b1 `compare` b2 <> c1 `compare` c2

-- 2.2. Циклический перебор.
-- Реализуйте класс типов CycleEnum. Обе его функции cycSucc и cycPred
-- ведут себя как succ и pred, но при достижении максимального и минимального
-- значений соответственно начинают сначала (то есть с минимального
-- и максимального соответственно).
-- Сделайте Bool представителем CycleEnum. Сделайте Int8 представителем CycleEnum.

class CycleEnum a where
  cycSucc :: a -> a
  cycPred :: a -> a

instance CycleEnum Bool where
  cycSucc = not
  cycPred = not

instance CycleEnum Int8 where
  cycSucc = \case
    127 -> -128
    x -> x+1
  cycPred = \case
    -128 -> 127
    x -> x-1

-- 2.3. Обёртка для циклического перебора.
-- Для newtype-обёртки Cycling реализуйте CycleEnum при условии, что оборачиваемый
-- тип удовлетворяет минимальному набору ограничений.

newtype Cycling a = Cycling a
  deriving newtype (Eq, Show, Bounded, Enum)

instance (Bounded a, Enum a, Eq a) => CycleEnum (Cycling a) where
  cycSucc x =
    if x == maxBound
      then minBound
      else succ x
  cycPred x =
    if x == minBound
      then maxBound
      else pred x



-- 2.4. Печать строк матрицы.
-- Matrix хранит список непустых строк; равенство длин строк НЕ требуется.
-- Для пустого списка выведите "EMPTY". Иначе каждую строку покажите как
-- обычный список, разделяя строки одним '\n', без завершающего '\n'.
-- Пример: Matrix [1 :| [2], 3 :| [4]] -> "[1,2]\n[3,4]".
-- Напишите инстанс Show целиком, с необходимыми ограничениями.
-- Можно использовать NE.toList и List.intercalate.

newtype Matrix a = Matrix [NonEmpty a]
  deriving Eq

instance Show a => Show (Matrix a) where
  show (Matrix x) =
    if null x
      then "EMPTY"
      else List.intercalate "\n" (show . NE.toList <$> x)

-- 2.5. Разберите показанную матрицу.
-- Для "EMPTY" верните Just (Matrix []). В остальных случаях каждая строка
-- входа должна целиком читаться как непустой список через readMaybe.
-- Если хотя бы одна строка не читается или пуста ([]), верните Nothing.
-- Пустая строка входа тоже ошибочна. Перевод строки в конце входа разрешён.
-- Закон: parseMatrix (show m) == Just m (для Int и Char).
-- Допишите parseRow; обвязка уже дана. Вспомните решение 1.3 из ДЗ 5.

parseMatrix :: Read a => String -> Maybe (Matrix a)
parseMatrix raw = case lines raw of
  [] -> Nothing
  ["EMPTY"] -> Just (Matrix [])
  rows -> fmap Matrix (parseRows rows)
  where
    parseRows :: Read b => [String] -> Maybe [NonEmpty b]
    parseRows [] = Just []
    parseRows (row : rest) = case parseRow row of
      Nothing -> Nothing
      Just parsedRow -> case parseRows rest of
        Nothing -> Nothing
        Just parsedRest -> Just (parsedRow : parsedRest)

    parseRow :: forall b. Read b => String -> Maybe (NonEmpty b)
    parseRow x = readMaybe @[b] x >>= NE.nonEmpty

-- 2.6. Сравнение структуры пары.
-- Сделайте пару представителем StructEq из задачи 1.4.
-- Для пары (e, a) форма включает фиксированную первую компоненту e:
-- сравните её через Eq, а второй компонентой пренебрегите.
-- Напишите инстанс целиком, выбрав необходимые ограничения.

instance Eq e => StructEq ((,) e) where
  (a, _) ==# (b, _) = a == b

-- 2.7. Функтор внутри списка, функции и пары.
-- Преобразуется только a. Длина и порядок списка, окружение e и метка p
-- сохраняются. Напишите инстанс целиком, используя
-- стандартные инстансы этих структур.
-- deriving Functor вместо собственного инстанса использовать нельзя.
-- Подсказка: (->) e действует посткомпозицией, (,) p меняет второй компонент.

newtype PairReaderList e p a = PairReaderList
  { getPairReaderList :: [e -> (p, a)] }

instance Functor (PairReaderList e p) where
  fmap f (PairReaderList g) = PairReaderList $ fmap (fmap f) <$> g
