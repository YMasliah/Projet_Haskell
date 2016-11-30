## Introduction

  Le but du projet est d'implementer l'algorithme de compression et de decompression de **Lempel-Ziv-Welch** avec differentes structures de donnes comme table de traduction, qui associe des chaines de caracetre a des codes (dans notre cas des entiers).
  Les deux structures que nous avons implementer sont les listes associatives et les arbres de prefixes, nous avons pas eu le temps pour l'instant de finir la partie avec les bits.

## Structuration du Projet

          \Projet_Haskell
                    Main_WO_Bits.hs
                    README.md
                    lzwDecode
                    lzwEncode
                    
  Un fichier `README.md` pour le rendu du projet ainsi que les etapes a suivre pour executer et tester les fonctionnalitees, les deux fichier executables `lzwEnCode` et `lzwDecode` sert a coder ou decoder un fichier.Et enfin le fichier principale `Main_WO_Bits.hs` qui contient les focntions et les ou on pourra tester notre travail.

## Un peu de Code

La fonction `empty :: a` retourne soit une table vide soit une liste associative soit un arbe de prefixe vide.

    *Main> empty :: ListeAssociative 
    L []
    *Main> empty :: Apref 
    Apref []

La fonction `ajouter :: a -> String -> a` fait que l'ajout d'un mot dans la table avec un code associe a cette chaine de caractere bien sure si il n'existe pas.

  Pour la liste associative elle verifie si la table est vide sinon elle verifie avec `isIn` si le mot en entree exsiste deja dans la table et si c'est pas le cas elle le rajoute a la liste.
  
  Et pour l'arbre de prefixe on fait la meme verification de l'existance du mot et sinon  on appelle la fonction `nbnoeud` qui calcucule recurivement le nombre total des noueds de l'arbre pour connaitre le code du prochain noeud et ainsi le retourner et l'utiliser dans une autre fonction recursive `ajouterR` qui cherche le premier caractere dans l'arbre et des qu'elle trouve le noeud qui le contient elle cree un nouvel arbre avec le reste de la chaine et ainsi de suite jusqu'a sque le reste de la chaine soit vide, chauqe nouveau noeud creer contient le code calculer avec `nbnoeud`. 
  
      *Main> ajouter charsMin "zz"
      L [(0,"a"),(1,"b"),(2,"c"),(4,"d"),(5,"e"),(6,"f"),(7,"g"),(8,"h"),(9,"i"),(10,"j"),(11,"k"),(12,"l"),(13,"m"),(14,"n"),(15,"o"),(16,"p"),(17,"q"),(18,"r"),(19,"s"),(20,"t"),(21,"u"),(22,"v"),(23,"w"),(24,"x"),(25,"y"),(26,"z"),(27," "),(28,"zz")]
      (0.01 secs, 0 bytes)
      *Main> ajouter arbreChar "zz"
      Apref [('a',0,Apref []),('b',1,Apref []),('c',2,Apref []),('d',3,Apref []),('e',4,Apref []),('f',5,Apref []),('g',6,Apref []),('h',7,Apref []),('i',8,Apref []),('j',9,Apref []),('k',10,Apref []),('l',11,Apref []),('m',12,Apref []),('n',13,Apref []),('o',14,Apref []),('p',15,Apref []),('q',16,Apref []),('r',17,Apref []),('s',18,Apref []),('t',19,Apref []),('u',20,Apref []),('v',21,Apref []),('w',22,Apref []),('x',23,Apref []),('y',24,Apref []),('z',25,Apref [('z',27,Apref [])]),(' ',26,Apref [])]
      (0.01 secs, 0 bytes)

La fonction `codeOf :: a -> String -> Maybe Code` qui retourne le code d'un mot est implementer pour la liste associative on utilisant le meme principe de `lookup` c-a-d chercher par recursivite le mot et retourner le code associe.

  Pour les arbres

      *Main> codeOf charsMin "z"
      Just 26
      (0.00 secs, 0 bytes)
      *Main> codeOf arbreChar "z"
      Just 25
      (0.00 secs, 0 bytes)

La fonction `stringOf :: a -> Code -> Maybe String`

La fonction `isIn :: a -> String -> Bool`

La fonction `split :: a -> String -> (String,Maybe Code,String)` 

## Compression et decompression

**Compression**
  On démarre avec une table contenant tous les caractères individuellement, avec leurs traductions. On parcourt
un flux de caractères, en lisant dans le flux, le plus long préfixe w contenu dans la table. On écrit alors en
sortie le code de ce préfixe. Si le flux n’est pas terminé, on itère, mais auparavant on ajoute à la table le mot
wc (avec un nouveau code), où c est le prochain caractère du flux.

**Décompression**
  On s’aperçoit qu’on peut décompresser un flux de codes en reconstruisant la table de traduction à fur
et mesure. On démarre avec la table contenant tous les caractères (et leur code associé), comme pour la
compression.
  Quand on lit un code du flux, on cherche son image inverse dans la table, c’est-à-dire la chaîne auquel ce
code est attribué, et on produit cette chaîne comme résultat partiel. On itère avec le reste du flux, mais en se
rappelant que la construction du flux de codes a étendu la table de traduction en ajoutant la correspondance
entre ce mot et le premier caractère du décodage du prochain code à venir.
  Il se pose un problème, car en principe nous ne savons pas décoder le prochain code (qui pourrait ne pas être
un le code d’un mot dans la table).

    lzwEncode :: Table a => a -> String -> [Code]

    lzwDecode :: Table a => a  -> [Code] -> String

    lzw_Decode :: Table a => a  -> String -> [Code] -> String

## Testes

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



    dico :: ListeAssociative
    dico = L [(i,[(toEnum i :: Char)])|i<-[0..255]]

    mytest = quickCheck ((\s -> (lzwDecode dico (lzwEncode dico s)) == s) :: [Char] -> Bool)
