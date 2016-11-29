import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.Word
import Data.Word.Odd

-- nous pouvons utiliser Word10 car nous utilisons seulement les valeurs int positives
type Bytes = Word8
type Int10 = Word10

class Table a where
  empty :: a 
  ajouter :: a -> Bytes -> a
  codeOf :: a -> Bytes -> Maybe Int10
  stringOf :: a -> Int10 -> Maybe Bytes
  isIn :: a -> Bytes -> Bool
  split :: a -> Bytes -> (Bytes,Maybe Int10,Bytes)
          
newtype ListeAssociative = L [(Int10,Bytes)]
                deriving Show

instance Table ListeAssociative where
  empty = (L []) 
  ajouter (L []) b = (L [(0,b)])
  ajouter (L a) b = (L (a ++ [(fst(last a)+1,b)]))
  codeOf (L a) b = lookupbis b a
  stringOf (L a) b = lookup b a
  isIn (L a) b = if codeOf (L a) b == Nothing then False else True
  split (L a) b =  (prefix, prefixCode, suffixe) 
        where
          prefix = findIsInMax (L a) b []
          prefixCode = codeOf (L a) prefix
          suffixe = drop (length prefix) b

--lookup sur le premier argument au lieu du second  
lookupbis                  :: (Eq b) => b -> [(a,b)] -> Maybe a
lookupbis _key []          =  Nothing
lookupbis  key ((x,y):xys)
    | key == y          =  Just x
    | otherwise         =  lookupbis key xys  

    
-- ci dessous toute les fonctions polymorphes avec toute les tables car elles utilisent seulement des methodes instancié pour la modification des tables    
-- cherche le plus grand string que connais la table
findIsInMax :: Table a => a -> Bytes -> Bytes -> Bytes
findIsInMax table [] charIsIn = charIsIn
findIsInMax table (char:string) charIsIn = if isIn table word == True then findIsInMax table string word else charIsIn
  where
    word = (charIsIn ++ [char])
    
lzwEncode :: Table a => a -> Bytes -> [Int10]
lzwEncode table bdd = 
  if suffixe == [] 
    then [prefixCode]
    else [prefixCode] ++ lzwEncode (ajouter table (prefix ++ [(head suffixe)])) suffixe 
  where
    (prefix,Just prefixCode,suffixe) = split table bdd 

lzwDecode :: Table a => a  -> [Int10] -> Bytes
lzwDecode table (code:codes) = word ++ lzw_Decode table word codes
  where
    Just word = stringOf table code
    
lzw_Decode :: Table a => a  -> Bytes -> [Int10] -> Bytes
lzw_Decode table prefix [] = []
lzw_Decode table prefix (code:codes) = 
  if isNothing wordTemp 
    then currentWord ++ lzw_Decode currentTable currentWord codes
    else newWord ++ lzw_Decode newTable newWord codes
  where
    wordTemp = stringOf table code
    newWord = fromJust wordTemp
    newTable = ajouter table (prefix ++ [(head newWord)])
    currentWord = (prefix ++ [(head prefix)])
    currentTable = ajouter table currentWord

-- la zone de test 

genSafeChar :: Gen Char
genSafeChar = elements ['a'..'c']
    
genSafeString :: Gen Bytes
genSafeString = vectorOf 20 genSafeChar    
    
test :: ListeAssociative -> ListeAssociative 
test (L a) = ajouter (ajouter (L a) "tata") "toto"
{-
grosTest :: ListeAssociative -> [Bool]
grosTest (L a) = replicate 20 (if string == string2 then True else False) 
  where
    string = genSafeString
    string2 = lzwDecode (L a) (lzwEncode (L a) string)
-}

charsMaj :: ListeAssociative
charsMaj = L [(0,"A"),(1,"B"),(2,"C"),(4,"D"),(5,"E"),(6,"F"),(7,"G"),(8,"H"),(9,"I"),(10,"J"),(11,"K"),(12,"L"),(13,"M"),(14,"N"),(15,"O"),(16,"P"),(17,"Q"),(18,"R"),(19,"S"),(20,"T"),(21,"U"),(22,"V"),(23,"W"),(24,"X"),(25,"Y"),(26,"Z")]

charsMin :: ListeAssociative
charsMin = L [(0,"a"),(1,"b"),(2,"c"),(4,"d"),(5,"e"),(6,"f"),(7,"g"),(8,"h"),(9,"i"),(10,"j"),(11,"k"),(12,"l"),(13,"m"),(14,"n"),(15,"o"),(16,"p"),(17,"q"),(18,"r"),(19,"s"),(20,"t"),(21,"u"),(22,"v"),(23,"w"),(24,"x"),(25,"y"),(26,"z")]

charsIzi :: ListeAssociative
charsIzi = L [(0,"a"),(1,"b"),(2,"c")]

string1 = "bonjourjemappelleyannmasliah"
string2 = "voicimonbinomeredouanetigrara"
string3 = "noussommesentraindefairelestest"
string4 = "surleprojetdehaskell"
string5 = "normalementtoutlestestdevraient"
string6 = "marchersurlarbreetlalisteassociative"
string7 = "nousnavonspascomprislhistoiredesbytes"
string8 = "pourlemomentmaispeutetreoncomprendra"
string9 = "avantlerenduquiseferaledernierjourde"
string10 = "novembrequiestunjeudi"
string11 = (string1 ++ string2 ++ string3 ++ string4 ++ string5 ++ string6 ++ string7 ++ string8 ++ string9 ++ string10)
string12 = string11 ++ string11
string13 = string12 ++ string12
string14 = string13 ++ string13
string15 = string14 ++ string14
string16 = string15 ++ string15
string17 = string16 ++ string16
string18 = string17 ++ string17
string19 = string18 ++ string18
string20 = string19 ++ string19


--arbre3 :: Apref
--arbre3 = Apref [(elements ['a'..'z'],elements [0..26],(Apref []))]

-- a partir du test 19 sa commence a etre tres long pour la liste associative
{- voici des test des 2 types, nous pouvons voir que l'encodage est beaucoup plus rapide avec l'arbre et beaucoup plus lent en decodage (par rapport a la liste associative)
*Main> let a = lzwEncode charsMin string18 in a==a
True
(32.14 secs, 8,048,460,088 bytes)
*Main> let a = lzwEncode arbreChar string18 in a==a
True
(12.12 secs, 4,300,215,536 bytes)
*Main> test18 charsMin
True
(33.61 secs, 8,631,901,936 bytes)
*Main> test18 arbreChar
True
(75.30 secs, 24,234,537,648 bytes)
*Main> let a = lzwEncode arbreChar string20 in a==a
True
(56.08 secs, 19,658,114,232 bytes)
*Main> let a = lzwEncode charsMin string20 in a==a
True
(244.45 secs, 61,103,861,384 bytes)
-}
{- test sur la V2 de l'arbre
*Main> test16 arbreChar
True
(7.00 secs, 2,288,803,128 bytes)
*Main> test18 arbreChar
True
(32.70 secs, 10,924,613,928 bytes)
*Main> test20 arbreChar
True
(143.97 secs, 48,084,181,176 bytes)
*Main> test20 charsMin
True
(254.38 secs, 63,607,594,816 bytes)
-}

test1 a = (lzwDecode a (lzwEncode a string1)) == string1
test2 a = (lzwDecode a (lzwEncode a string2)) == string2
test3 a = (lzwDecode a (lzwEncode a string3)) == string3
test4 a = (lzwDecode a (lzwEncode a string4)) == string4
test5 a = (lzwDecode a (lzwEncode a string5)) == string5
test6 a = (lzwDecode a (lzwEncode a string6)) == string6
test7 a = (lzwDecode a (lzwEncode a string7)) == string7
test8 a = (lzwDecode a (lzwEncode a string8)) == string8
test9 a = (lzwDecode a (lzwEncode a string9)) == string9
test10 a = (lzwDecode a (lzwEncode a string10)) == string10

test11 a = (lzwDecode a (lzwEncode a string11)) == string11
test12 a = (lzwDecode a (lzwEncode a string12)) == string12
test13 a = (lzwDecode a (lzwEncode a string13)) == string13
test14 a = (lzwDecode a (lzwEncode a string14)) == string14
test15 a = (lzwDecode a (lzwEncode a string15)) == string15
test16 a = (lzwDecode a (lzwEncode a string16)) == string16
test17 a = (lzwDecode a (lzwEncode a string17)) == string17
test18 a = (lzwDecode a (lzwEncode a string18)) == string18
test19 a = (lzwDecode a (lzwEncode a string19)) == string19
test20 a = (lzwDecode a (lzwEncode a string20)) == string20

testUltime a = test1 a && test2 a && test3 a && test4 a && test5 a && test6 a && test7 a && test8 a && test9 a && test10 a && test11 a && test12 a && test13 a && test14 a && test15 a && test16 a && test17 a && test18 a && test19 a && test20 a
{-
*Main> testUltime charsMin
True
(400.56 secs, 100,704,230,024 bytes)
*Main> testUltime arbreChar
True
(274.41 secs, 91,104,684,136 bytes)
-}