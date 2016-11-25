import Test.QuickCheck

type Code = Int

data Apref = Apref [(Code,Char,Apref)] deriving (Show,Eq)

class Table a where
	empty :: a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String,Maybe Code,String)

instance Table Apref where
	empty = (Apref []) 
	ajouter (Apref a) [b] = (Apref (a ++ [(0,b,empty)]))
	ajouter (Apref (a:autres)) (d:bs) = if b == d then ajouter c bs else ajouter (Apref autres) (d:bs)
		where
			(_,b,c) = a 
--ajouter ECHECCC TRY AGAIN ! xD
{-	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String,Maybe Code,String)

findIsInMax :: ListeAssociative -> String -> String -> String
findIsInMax (L a) [] c = c
findIsInMax (L a) (b:bs) c = if isIn (L a) word == True then findIsInMax (L a) bs word else c
	where
		word = (c ++ [b])
	
lookupbis                  :: (Eq b) => b -> [(a,b)] -> Maybe a
lookupbis _key []          =  Nothing
lookupbis  key ((x,y):xys)
    | key == y          =  Just x
    | otherwise         =  lookupbis key xys-}

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
findIsInMax (L a) (b:bs) c = if isIn (L a) word == True 
									then findIsInMax (L a) bs word
									else c
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

lzwEncode :: Table a => a -> String -> [Code]
lzwEncode table bdd = if suffixe == [] 
											then [prefixCode] --ajouter (L table) prefix
											else [prefixCode] ++ lzwEncode (ajouter table (prefix ++ [(head suffixe)])) suffixe 
				where
					(prefix,Just prefixCode,suffixe) = split table bdd 
					
fromJust          :: Maybe a -> a
fromJust (Just x) = x

lzwDecode :: Table a => a  -> [Code] -> String
lzwDecode table (code:codes) = word ++ lzw_Decode table word codes
	where
		Just word = stringOf table code
		
lzw_Decode :: Table a => a  -> String -> [Code] -> String
lzw_Decode table prefix [] = [] --ajouter (L table) prefix
lzw_Decode table prefix (code:codes) = if word == Nothing 
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
valeur :: ListeAssociative
valeur = L [(0,"a"),(1,"b"),(2,"c")]

code :: [Int]
code = [0,1,3,2,4,7,0,9,10,0]

string = "ababcbababaaaaaaa"

arbre :: Apref
arbre = Apref [(0,'a',(Apref [(4,'b',(Apref []))])),(1,'b',(Apref [])),(3,'c',(Apref[]))]

