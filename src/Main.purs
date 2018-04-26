module Main where

import Prelude
import Control.Apply hiding (lift2, lift3)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic
import Data.Generic.Rep.Show (genericShowFields)
import Data.Identity (Identity(..))
import Data.String (joinWith)
import Data.Maybe (maybe)
import Unsafe.Coerce (unsafeCoerce)
import Data.Foreign
import Data.Foreign.Class
import Data.Foreign.Generic
import Data.Foreign.JSON
import Control.Monad.Except

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
type Error = String


---------- syntax differences ----------
-- | type annotations
add :: Int -> Int -> Int
add a b = a + b

-- | type aliases
type Pixels = Number
type Result a = Either Error a
type Address =
  { street  :: String
  , city    :: String
  , country :: String
  }

data PandaSpecies = Red | Giant
data Coords = Coords Int Int

newtype Inches = Inches Number


-- | literals
x = true :: Boolean

y = false :: Boolean


x = 12 :: Int

y = 123.123 :: Number -- note: floating point JS number


x = "hello world" :: String

y :: String
y =
  """
  multiline
  """

-- | lists
x = [1,2,3] :: Array -- an actual Javascript array

-- Data.List
data List a
  = Nil
  | Cons (List a)


-- | Records
-- type Person = { name :: String, age :: Int }

-- x :: Person
-- x = { name: "Abby", age: 100 }


-- | Functions
double :: Int -> Int
double n = n * 2

data Path
  = Directory String (Array Path)
  | File String Int

isJust :: forall a. Maybe a -> Boolean
isJust (Just _) = true
isJust Nothing = false

fromMaybe :: Maybe a -> List a
fromMaybe (Just xs) = xs
fromMaybe Nothing = []

panicButton :: Int -> String
panicButton powerLevel
  | powerLevel > 9000 = "OVER 9000!!!"
  | otherwise = "meh"

-- | Modules

-- module MyModule
--   ( exported
--   ) where

-- import Prelude
-- import Data.Array as A
-- import Data.List (head)
-- import Data.Map hiding (singleton)


---------- Maybe ----------
-- PS --
-- | The `Maybe` type is used to represent optional values and can be seen as
-- | something like a type-safe `null`, where `Nothing` is `null` and `Just x`
-- | is the non-null value `x`.
data Maybe a = Nothing | Just a


-- Elm --
-- {-| Represent values that may or may not exist. It can be useful if you have a
-- record field that is only filled in sometimes. Or if a function takes a value
-- sometimes, but does not absolutely need it.
--     -- A person, but maybe we do not know their age.
--     type alias Person =
--         { name : String
--         , age : Maybe Int
--         }
--     tom = { name = "Tom", age = Just 42 }
--     sue = { name = "Sue", age = Nothing }
-- -}
-- type Maybe a
--     = Just a
--     | Nothing

---------- Either ----------
-- PS --
-- | The `Either` type is used to represent a choice between two types of value.
-- |
-- | A common use case for `Either` is error handling, where `Left` is used to
-- | carry an error value and `Right` is used to carry a success value.
data Either a b = Left a | Right b

-- | Takes a default value, and a `Maybe` value. If the `Maybe` value is
-- | `Nothing` the default value is returned, otherwise the value inside the
-- | `Just` is returned.
-- |
-- | ``` purescript
-- | fromMaybe x Nothing == x
-- | fromMaybe x (Just y) == y
-- | ```
fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe a = maybe a id


-- Elm --
-- {-|
-- A generic structure for a type with two possibilities: a `Left a` or
-- a `Right b`.
-- An `Either` is right-biased, so most operations will be applied to
-- the `Right`.
-- This is similar to `Result` type in `core`, but is more generic.
-- If your looking for a data type to do error handling, you should
-- use `Result` instead.
-- # Definition
-- @docs Either
-- # Mapping
-- @docs map, map2, map3, mapLeft, mapRight, mapBoth, mapDefault
-- # Singleton & Applying
-- @docs singleton, andMap
-- # Chaining
-- @docs andThen
-- # List Helpers
-- @docs lefts, rights, partition
-- # Maybe Helpers
-- @docs toMaybe, leftToMaybe, rightToMaybe, fromMaybe, leftFromMaybe, rightFromMaybe
-- # Result Helpers
-- @docs toResult, fromResult
-- # Rest of the Helpers
-- @docs isLeft, isRight, fromLeft, fromRight, withDefault, elim, either, swap
-- -}

-- type Either a b
--     = Left a
--     | Right b

-- {-| Provide a default value, turning an optional value into a normal
-- value.  This comes in handy when paired with functions like
-- [`Dict.get`](Dict#get) which gives back a `Maybe`.
--     withDefault 100 (Just 42)   -- 42
--     withDefault 100 Nothing     -- 100
--     withDefault "unknown" (Dict.get "Tom" Dict.empty)   -- "unknown"
-- -}
-- withDefault : a -> Maybe a -> a
-- withDefault default maybe =
--     case maybe of
--       Just value -> value
--       Nothing -> default


---------- typeclasses ----------
-- https://github.com/purescript/documentation/blob/master/language/Type-Classes.md

class Show a where
  show :: a -> String

instance showString :: Show String where
  show s = s

instance showBoolean :: Show Boolean where
  show true = "true"
  show false = "false"

instance showArray :: (Show a) => Show (Array a) where
  show xs = "[" <> joinWith ", " (map show xs) <> "]"

example = show [true, false]


---------- typeclass deriving ---------
newtype Person = Person { name :: String, age :: Int }

derive instance eqPerson :: Eq Person
derive instance ordPerson :: Ord Person


newtype FormData = FormData
  { street  :: String
  , city    :: String
  , country :: String
  }

--parseJSON :: String -> F Foreign
--decodeJSON :: forall a. Decode a => String -> F a

derive instance genericFormData :: Generic FormData

instance decodeFormData :: Decode FormData where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeFormData :: Encode FormData where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })


-- > runExcept (decodeJSON "\"Testing\"" :: F String)
-- Right "Testing"

-- > runExcept (decodeJSON "true" :: F Boolean)
-- Right true

-- > runExcept (decodeJSON "[1, 2, 3]" :: F (Array Int))
-- Right [1, 2, 3]

---------- lift / map ----------
-- PS --
-- class Functor f <= Apply f where
--   apply :: forall a b. f (a -> b) -> f a -> f b

-- -- | Lift a function of two arguments to a function which accepts and returns
-- -- | values wrapped with the type constructor `f`.
-- lift2 :: forall a b c f. Apply f => (a -> b -> c) -> f a -> f b -> f c
-- lift2 f a b = f <$> a <*> b

-- -- | Lift a function of three arguments to a function which accepts and returns
-- -- | values wrapped with the type constructor `f`.
-- lift3 :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
-- lift3 f a b c = f <$> a <*> b <*> c

-- Elm --
-- {-| Transform a `Maybe` value with a given function:
--     map sqrt (Just 9) == Just 3
--     map sqrt Nothing  == Nothing
--     map sqrt (String.toFloat "9") == Just 3
--     map sqrt (String.toFloat "x") == Nothing
-- -}
-- map : (a -> b) -> Maybe a -> Maybe b
-- map f maybe =
--   case maybe of
--     Just value ->
--       Just (f value)

--     Nothing ->
--       Nothing


-- {-| Apply a function if all the arguments are `Just` a value.
--     map2 (+) (Just 3) (Just 4) == Just 7
--     map2 (+) (Just 3) Nothing == Nothing
--     map2 (+) Nothing (Just 4) == Nothing
--     map2 (+) (String.toInt "1") (String.toInt "123") == Just 124
--     map2 (+) (String.toInt "x") (String.toInt "123") == Nothing
--     map2 (+) (String.toInt "1") (String.toInt "1.3") == Nothing
-- -}
-- map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
-- map2 func ma mb =
--   case ma of
--     Nothing ->
--       Nothing

--     Just a ->
--       case mb of
--         Nothing ->
--           Nothing

--         Just b ->
--           Just (func a b)

---------- rank n types ----------
-- eg. credit https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
-- monomorphic
intId :: Int -> Int
intId x = x

numberId :: Number -> Number
numberId x = x

-- parametric poly
id :: forall a. a -> a
id x = x
-- parametricity: all instances of a fn work the same way
-- handles different values identically
-- allows us to abstract types from values
-- this fn is "parameterised" by a for all vals of a
-- makes lang more expressive w/o sacrificing type safety
-- when calling, instantiate to a concrete type
-- eg. id (3 :: Int)

-- Rank-1 poly
-- can extract sig out
type IdFunc = forall a. a -> a
-- bc type alias can use as part of fn sig
id' :: IdFunc
id' x = x
-- can be used as domain of fn
intId' :: IdFunc -> Int
intId' id' = id' 3
-- we then apply that polymorphic fn to Int, once applied goes from poly to monomorphic
-- aka
intId' :: (forall a. a -> a) -> Int
intId' id' = id' 3
-- call "rank 1" based on level of parens


-- Rank-2 poly
type NestedIdFunc = IdFunc -> Int
doubleIntId'' :: IdFunc -> Int
doubleIntId'' intId' = intId' id + intId' id
-- aka
doubleIntId'' :: ((forall a. a -> a) -> Int) -> Int
doubleIntId'' intId' = intId' id + intId' id
-- "rank 2" as nested in 2 levels of parens

-- TODO component example?
-- The idea of higher-rank types is to make polymorphic functions first-class

---------- higher-kinded types ----------
-- data Maybe a = Nothing | Just a
-- Maybe: type constructor
-- Just: data constructor
-- same, but one level "higher"
-- "kind" type of types? https://stackoverflow.com/questions/37369251/in-haskell-are-higher-kinded-types-really-types-or-do-they-merely-denote-c
-- kind: Maybe :: * -> *

-- really nice example from https://stackoverflow.com/questions/48523571/whats-the-difference-between-parametric-polymorphism-and-higher-kinded-types?rq=1
type Ground = Int
type FirstOrder a = Maybe a  -- a is ground
type SecondOrder c = c Int   -- c is a first-order constructor
type ThirdOrder c = c Maybe  -- c is second-order

-- f :: forall c. c Int -> c Int  -- c is a constructor

-- real world example from https://stackoverflow.com/questions/42370444/what-types-of-problems-helps-higher-kinded-polymorphism-solve-better
data Opt = Opt
   { opt1 :: Boolean
   , opt2 :: String
   -- many other fields here
   }

-- merge all options in one single configuration, or fail
finalize :: Array TempOpt -> Maybe Opt
finalize = unsafeCoerce unit

-- convert to higher-kinded, reduce dupe
data FOpt f = FOpt
   { opt1 :: f Boolean
   , opt2 :: f String
   -- many other fields here
   }
type Opt' = FOpt Identity
type TempOpt = FOpt Maybe

-- as before: merge all options in one single configuration, or fail
finalize' :: Array TempOpt -> Maybe Opt'
finalize' = unsafeCoerce unit

-- Higher-Kinded Types http://dev.stephendiehl.com/fun/001_basics.html#higher-kinded-types
-- The "type of types" in Haskell is the language of kinds. Kinds are either an arrow (k -> k') or a star (*).
-- The kind of Int is *, while the kind of Maybe is * -> *. Haskell supports higher-kinded types, which are types that take other types and construct a new type. A type constructor in Haskell always has a kind which terminates in a *.

-- TODO higher-kinded polymorphism
