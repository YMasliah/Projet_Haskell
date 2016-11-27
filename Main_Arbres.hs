import Test.QuickCheck
import Data.Maybe
import Data.Char

type Code = Int

data Apref = Apref [(Char,Code,Apref)] deriving (Show,Eq)

class Table a where
  empty :: a 
  ajouter :: a -> String -> a
  codeOf :: a -> String -> Maybe Code
  stringOf :: a -> Code -> Maybe String
  isIn :: a -> String -> Bool
  split :: a -> String -> (String,Maybe Code,String)

instance Table Apref where
  empty = (Apref []) 
  ajouter (Apref arbre) chaine = ajouterR (Apref arbre) chaine code
    where
      code = nbnoeud (Apref arbre)
  codeOf (Apref []) _ = Nothing
  codeOf (Apref ((char,b,_):autres)) [lettre] = if lettre == char then Just b else codeOf (Apref autres) [lettre]
  codeOf (Apref (noeud:autres)) (char:chaine) = if a == char then codeOf c chaine else codeOf (Apref autres) (char:chaine) 
    where
    (a,b,c) = noeud 
  stringOf (Apref a) code = stringOfR (Apref a) code []
  isIn (Apref arbre) string = if codeOf (Apref arbre) string == Nothing then False else True
  split (Apref arbre) string = (prefix, prefixCode, suffixe)
        where
          prefix = findIsInMax (Apref arbre) string []
          prefixCode = codeOf (Apref arbre) prefix
          suffixe = drop (length prefix) string  

-- decoupe l'arbre en sous arbre et cherche dans chaque sous arbre si il y as le code.
-- chaque fois qu'il avance d'une profondeur dans l'arbre il enregistre la lettre qu'il y as dans le 3plet
-- la fonction a une complexité tres eleve, n^3 surement. il doit y avoir une methode beaucoup plus rapide.
-- Edit : V2 faite, c'est plus que n^2 je crois, maintenant il est plus rapide que la liste associative (test a la fin du fichier)
stringOfR :: Apref -> Code -> String -> Maybe String
stringOfR (Apref []) _ _ = Nothing
stringOfR (Apref ((char,codebis,noeud):autres)) code string = if code == codebis then Just (string ++ [char]) else if isJust noeuds then noeuds else if isJust branches then branches else Nothing  
    where
        branches = stringOfR (Apref autres) code string 
        noeuds = stringOfR noeud code (string ++ [char])
-- la fonction ajouter de l'arbre avec l'information du code final en argument    
ajouterR :: Apref -> String -> Code -> Apref
ajouterR (Apref noeud) [lettre] code = (Apref (noeud ++ [(lettre,code,empty)]))
ajouterR (Apref (branche:autres)) (char:chaine) code = if a == char then Apref ((a,b,(ajouterR c chaine code)):autres) else (Apref ([branche] ++ sousArbre))
  where
  (Apref sousArbre) = ajouterR (Apref autres) (char:chaine) code
  (a,b,c) = branche
    
-- fonction qui compte le nombre de noeud dans l'arbre pour connaitre le code du prochain noeud
nbnoeud :: Apref -> Code 
nbnoeud (Apref []) = 0
nbnoeud (Apref (d:autres)) = 1 + nbnoeud c + nbnoeud (Apref autres)
    where
          (a,b,c) = d 
          
newtype ListeAssociative = L [(Code,String)]
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
findIsInMax :: Table a => a -> String -> String -> String
findIsInMax table [] charIsIn = charIsIn
findIsInMax table (char:string) charIsIn = if isIn table word == True then findIsInMax table string word else charIsIn
  where
    word = (charIsIn ++ [char])
    
lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table bdd = 
  if suffixe == [] 
    then [prefixCode]
    else [prefixCode] ++ lzwEncode (ajouter table (prefix ++ [(head suffixe)])) suffixe 
  where
    (prefix,Just prefixCode,suffixe) = split table bdd 

lzwDecode :: Table a => a  -> [Code] -> String
lzwDecode table (code:codes) = word ++ lzw_Decode table word codes
  where
    Just word = stringOf table code
    
lzw_Decode :: Table a => a  -> String -> [Code] -> String
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
    
genSafeString :: Gen String
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

arbre :: Apref
arbre = Apref [('a',0,(Apref [])),('b',1,(Apref [])),('c',2,(Apref[]))]

string = "ababcbababaaaaaaa"

code :: [Int]
code = [0,1,3,2,4,7,0,9,10,0]

arbreChar :: Apref
arbreChar = Apref [('a',0,(Apref [])),('b',1,(Apref [])),('c',2,(Apref[])),('d',3,(Apref [])),('e',4,(Apref [])),('f',5,(Apref[])),('g',6,(Apref [])),('h',7,(Apref [])),('i',8,(Apref[])),('j',9,(Apref [])),('k',10,(Apref [])),('l',11,(Apref[])),('m',12,(Apref [])),('n',13,(Apref [])),('o',14,(Apref[])),('p',15,(Apref [])),('q',16,(Apref [])),('r',17,(Apref[])),('s',18,(Apref [])),('t',19,(Apref [])),('u',20,(Apref[])),('v',21,(Apref [])),('w',22,(Apref [])),('x',23,(Apref[])),('y',24,(Apref [])),('z',25,(Apref []))]

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

--testUltime = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9 && test10 && test11 && test12 && test13 && test14 && test15 && test16 && test17 && test18 && test19 && test20
