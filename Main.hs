import Data.Char
import Test.QuickCheck

type Code = Int

class Table a where
	empty :: a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String,Maybe Code,String)

newtype ListeAssociative = L [(Code,String)]
													deriving Show

instance Table ListeAssociative where
	empty = (L []) 
	ajouter (L []) b = (L [(0,b)])
	ajouter (L a) b = (L (a ++ [(fst(last a)+1,b)]))
	codeOf (L a) b = lookupbis b a
	stringOf (L a) b = lookup b a
	isIn (L a) b = if codeOf (L a) b == Nothing then False else True
	split (L a) b =	(prefix, prefixCode, suffixe) 
								where
									prefix = findIsInMax (L a) b []
									prefixCode = codeOf (L a) prefix
									suffixe = drop (length prefix) b
	
findIsInMax :: ListeAssociative -> String -> String -> String
findIsInMax (L a) [] c = c
findIsInMax (L a) (b:bs) c = if isIn (L a) word == True then findIsInMax (L a) bs word else c
	where
		word = (c ++ [b])
	
lookupbis                  :: (Eq b) => b -> [(a,b)] -> Maybe a
lookupbis _key []          =  Nothing
lookupbis  key ((x,y):xys)
    | key == y          =  Just x
    | otherwise         =  lookupbis key xys	
		
-- regarder le wikipedia  
-- le encode recupere une chaine de caractere
-- il regarde caractere par caractere, tant qu'il reconnais le couple de caractere il continue, d'es qu'il en trouve un nouveau le rajoute dans la table et lui met une valeur
--

lzwEncode :: ListeAssociative -> String -> [Code]
lzwEncode (L table) bdd = if suffixe == [] 
											then [prefixCode] --ajouter (L table) prefix
											else [prefixCode] ++ lzwEncode (ajouter (L table) (prefix ++ [(head suffixe)])) suffixe 
				where
					(prefix,Just prefixCode,suffixe) = split (L table) bdd 
					
fromJust          :: Maybe a -> a
fromJust (Just x) = x

lzwDecode :: ListeAssociative -> [Code] -> String
lzwDecode (L table) (code:codes) = word ++ lzw_Decode (L table) word codes
	where
		Just word = stringOf (L table) code
		
lzw_Decode :: ListeAssociative -> String -> [Code] -> String
lzw_Decode (L table) prefix [] = [] --ajouter (L table) prefix
lzw_Decode (L table) prefix (code:codes) = if word == Nothing 
																						then newWord ++ lzw_Decode (L newTable) newWord codes
																						else fromJust word ++ lzw_Decode (L newTable) (fromJust word) codes
	where
		word = stringOf (L table) code
		newWord = (prefix ++ [(head prefix)])
		(L newTable) = ajouter (L table) newWord
		

test :: ListeAssociative -> ListeAssociative 
test (L a) = ajouter (ajouter (L a) "tata") "toto"

valeur :: ListeAssociative
valeur = L [(0,"a"),(1,"b"),(2,"c")]
code :: [Int]
code = [0,1,3,2,4,7,0,9,10,0]
string = "ababcbababaaaaaaa"
