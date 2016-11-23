type Code = Int

class Table a where
	empty :: a 
	ajouter :: a -> String -> a
	codeOf :: a -> String -> Maybe Code
	stringOf :: a -> Code -> Maybe String
	isIn :: a -> String -> Bool
	split :: a -> String -> (String,Maybe Code,String)

newtype ListeAssociative = L [(Code,String)]

instance Table ListeAssociative where
	empty = L [] 
	ajouter L [] b = L [(0,b)]
	ajouter L a b = L (a ++ [(fst(last a)+1,b)])
	codeOf L a b = lookupbis b a
	stringOf L a b = lookup b a
	isIn L a b = if codeOf L a b == Nothing then False else True
	split L a b =	(prefix, prefixCode, suffixe) 
								where
									prefix = findIsInMax a b []
									prefixCode = codeOf a prefix
									suffixe = drop (length prefix) b
	
findisInMax :: Table a => a -> String -> String -> String
findIsInMax L a (b:bs) c = if isIn L a word == True then findIsInMax L a bs word else c
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
lzwEncode a b = if (e != "") then lzwEncode (ajouter L a c:e) es  
				where
					(c,d,e:es) = split a b 

lzwDecode :: Table a => a -> [Code] -> String
lzwDecode (a,b):ls (x:xs) = if (string == null) then lzw_Decode a    
				where
					string = stringOf(x)
					
					
					c = lzw_Decode a string xs


lzw_Decode :: Table a => a -> String -> [Code] -> String
lzw_Decode a b c =  ajouter a (stringOf a c)



