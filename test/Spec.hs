import Test.QuickCheck
import Lib
import Data.Either

prop_ReverseReverseId :: JSON -> Bool
prop_ReverseReverseId json =
  parseJSON (dumpJSON json) == Right json

instance Arbitrary JSON where
  arbitrary = arbitraryJson

arbitraryJson :: Gen JSON
arbitraryJson = do
  -- construtor <- choose (0, 6)
  -- t <- arbitrary
  -- n <- choose (0, m `div` 2)
  -- ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  -- return (Tree t ts)
  oneof [
      arbitraryNull
    , arbitraryJSNumber
    , arbitraryJSFloat
    , arbitraryJSString
    , arbitraryJSBool
    , sized arbitraryJSArray
    , sized arbitraryJSObject
    ]

arbitraryNull :: Gen JSON
arbitraryNull = return JSNull

arbitraryJSNumber :: Gen JSON
arbitraryJSNumber = do
    t <- arbitrary
    return (JSNumber t)

arbitraryJSFloat :: Gen JSON
arbitraryJSFloat = do
    t <- arbitrary
    return (JSFloat t)

arbitraryJSString :: Gen JSON
arbitraryJSString = do
    t <- arbitrary
    return (JSString t)

arbitraryJSBool :: Gen JSON
arbitraryJSBool = do
    t <- arbitrary
    return (JSBool t)

arbitraryJSArray :: Int -> Gen JSON
arbitraryJSArray m = do
    n <- choose (0, m `div` 2)
    seq <- sequence [ arbitraryJson | _ <- [1..n] ]
    return (JSArray seq)

arbitraryTuple :: Gen (String, JSON)
arbitraryTuple = do
    k <- arbitrary
    v <- arbitraryJson
    return (k, v)

arbitraryJSObject :: Int -> Gen JSON
arbitraryJSObject m = do
    n <- choose (0, m `div` 2)
    seq <- sequence [ arbitraryTuple | _ <- [1..n] ]
    return (JSObject seq)

-- JSNull
-- JSBool Bool
-- JSNumber Int
-- JSFloat Double
-- JSString String
-- JSArray [JSON]
-- JSObject [(String,JSON)]

main :: IO ()
main = quickCheck prop_ReverseReverseId