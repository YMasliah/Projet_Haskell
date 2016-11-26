import Test.QuickCheck
import Data.Maybe

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
  stringOf (Apref a) code = if string == [] then Nothing else Just string
    where
        string = stringOfR (Apref a) code 
  isIn (Apref arbre) string = if codeOf (Apref arbre) string == Nothing then False else True
  split (Apref arbre) string = (prefix, prefixCode, suffixe)
        where
          prefix = findIsInMax (Apref arbre) string []
          prefixCode = codeOf (Apref arbre) prefix
          suffixe = drop (length prefix) string  

-- decoupe l'arbre en sous arbre et cherche dans chaque sous arbre si il y as le code.
-- chaque fois qu'il avance d'une profondeur dans l'arbre il enregistre la lettre qu'il y as dans le 3plet
stringOfR :: Apref -> Code -> String    
stringOfR (Apref ((char,codebis,noeud):autres)) code = if code == codebis then [char] else if teststringOfR noeud code then [char] ++ (stringOfR noeud code) else if teststringOfR (Apref autres) code then (stringOfR (Apref autres) code) else []

-- Methode isIn avec un code en entrée
teststringOfR :: Apref -> Code -> Bool
teststringOfR (Apref []) _ = False
teststringOfR (Apref ((char,codebis,noeud):autres)) code = if code == codebis then True else (teststringOfR (Apref autres) code) || (teststringOfR noeud code)

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
  if word == Nothing 
  then newWord ++ lzw_Decode newTable newWord codes
  else fromJust word ++ lzw_Decode newTable (fromJust word) codes
  where
    word = stringOf table code
    newWord = (prefix ++ [(head prefix)])
    newTable = ajouter table newWord

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
charsMaj = L [(0,"A"),(1,"B"),(2,"C"),(4,"D"),(5,"E"),(6,"F"),(7,"G"),(8,"H"),(9,"I"),(10,"J"),(11,"K"),(12,"L"),(13,"M"),(14,"N"),(15,"O"),(16,"P"),(17,"Q"),(18,"R"),(19,"S"),(20,"T"),(21,"U"),(0,"V"),(22,"W"),(23,"X"),(24,"Y"),(25,"Z")]

charsMin :: ListeAssociative
charsMin = L [('0'..'25','a'..'z')]

code1 :: [Int]
code1 = [0,1,3,2,4,7,0,9,10,0]

string1 = "ababcbababaaaaaaa"

string2 = "ab"

arbre1 :: Apref
arbre1 = Apref [('a',0,(Apref [('b',3,(Apref []))])),('b',1,(Apref [])),('c',2,(Apref[]))]

arbre2 :: Apref
arbre2 = Apref [('a',0,(Apref [])),('b',1,(Apref [])),('c',2,(Apref[]))]






 