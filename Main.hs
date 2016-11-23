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
	ajouter L a b = L [("va savoir",b),a]
	codeOf L a b = lookupbis b a
	stringOf L a b = lookup b a
	isIn L a b = 1 && (lookupbis b a)
	split L a b = (b,1234,b)
	
lookupbis                  :: (Eq a) => a -> [(a,b)] -> Maybe b
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



