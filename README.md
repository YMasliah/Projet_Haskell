## Introduction

  Le but du projet est d'implementer l'algorithme de compression et de decompression de **Lempel-Ziv-Welch** avec differentes structures de donnes comme table de traduction, qui associe des chaines de caracetre a des codes (dans notre cas des entiers).
  Les deux structures que nous avons implementer sont les listes associatives et les arbres de prefixes, nous avons pas eu le temps pour l'instant de finir la partie avec les bits.

## Structuration du Projet

          \Projet_Haskell
                    Main_WO_Bits.hs
					Main_W_Bits.hs
                    README.md
                    lzwDecode
                    lzwEncode
                    
  Un fichier `README.md` pour le rendu du projet ainsi que les etapes a suivre pour executer et tester les fonctionnalitees, les deux fichier executables `lzwEnCode` et `lzwDecode` sert a coder ou decoder un fichier.Et enfin le fichier principale `Main_WO_Bits.hs` qui contient les focntions et les ou on pourra tester notre travail.

## Un peu de Code

La fonction `empty :: a` retourne la valeur du type sans element, chez nous un tableau vide.

    *Main> empty :: ListeAssociative 
    L []
    *Main> empty :: Apref 
    Apref []

La fonction `ajouter :: a -> String -> a` fait que l'ajout d'un mot dans la table avec un code associe a cette chaine de caractere bien sure si il n'existe pas.

  Pour la liste associative elle verifie avec `isIn` si le mot en entree existe deja dans la table et si c'est pas le cas elle le rajoute a la liste (le code etant le dernier argument).
  
  Et pour l'arbre de prefixe on fait la meme verification de l'existence du mot et sinon  on appelle la fonction `nbnoeud` qui calcucule recurcivement le nombre total des noeuds de l'arbre pour connaitre le code du prochain noeud et ainsi le retourner et l'utiliser dans une autre fonction recursive `ajouterR` qui parcours l'arbre en le recreant jusqu'a arriver au noeud où il doit rajouter la lettre et le code 
  
      *Main> ajouter charsMin "zz"
      L [(0,"a"),(1,"b"),(2,"c"),(4,"d"),(5,"e"),(6,"f"),(7,"g"),(8,"h"),(9,"i"),(10,"j"),(11,"k"),(12,"l"),(13,"m"),(14,"n"),(15,"o"),(16,"p"),(17,"q"),(18,"r"),(19,"s"),(20,"t"),(21,"u"),(22,"v"),(23,"w"),(24,"x"),(25,"y"),(26,"z"),(27," "),(28,"zz")]
      (0.01 secs, 0 bytes)
      *Main> ajouter arbreChar "zz"
      Apref [('a',0,Apref []),('b',1,Apref []),('c',2,Apref []),('d',3,Apref []),('e',4,Apref []),('f',5,Apref []),('g',6,Apref []),('h',7,Apref []),('i',8,Apref []),('j',9,Apref []),('k',10,Apref []),('l',11,Apref []),('m',12,Apref []),('n',13,Apref []),('o',14,Apref []),('p',15,Apref []),('q',16,Apref []),('r',17,Apref []),('s',18,Apref []),('t',19,Apref []),('u',20,Apref []),('v',21,Apref []),('w',22,Apref []),('x',23,Apref []),('y',24,Apref []),('z',25,Apref [('z',27,Apref [])]),(' ',26,Apref [])]
      (0.01 secs, 0 bytes)

La fonction `codeOf :: a -> String -> Maybe Code` qui retourne le code d'un mot qui est implementer : 
	Pour la liste associative on utilise le meme principe de `lookup` c-a-d chercher par recursivite le mot et retourner le code associe. 
	Pour les arbres la fonction fait un parcours recursif jusqu'a trouver toute la chaine de caractere dans l'arbre puis elle renvoi le code associer.

        *Main> codeOf charsMin "z"
        Just 25
        (0.00 secs, 0 bytes)
        *Main> codeOf arbreChar "z"
        Just 25
        (0.00 secs, 0 bytes)

La fonction `stringOf :: a -> Code -> Maybe String` fonctionne du principe inverse de `codeOf` :
	pour la liste elle fait juste un `lookup` du mot dans la liste. 
	pour les arbres on reconstruit la chaine de caractere avec un 3eme argument dans une fonction recurcive. si le code trouver est le bon alors le 3eme argument crée est le resultat

    *Main> stringOf arbreChar 2
    Just "c"
    (0.01 secs, 0 bytes)
    *Main> stringOf charsMin  2
    Just "c"
    (0.00 secs, 0 bytes)

Pour la fonction `isIn :: a -> String -> Bool` on a utiliser la fonction precedante `codeOf` , si il retourne nothing alors False sinon True

La fonction `split :: a -> String -> (String,Maybe Code,String)` a le meme principe pour les listes et pour les arbres c-a-d : 
	on utilise une fonction polymorphe recurcive `findIsInMax` qui utilise isIn sur une partie du String pour trouver le plus grand mot
	puis on utilise un codeOf pour recuperer son code
	puis on fait un drop du mot trouver sur le String a encoder (Suffixe)

## Compression et decompression

En utilisant toutes les fonctions et les implementants dans lzwEncode et lzwDecode nous pouvons faires des tests varies et differents mais on montre les plus important :

        *Main> let a = lzwEncode charsMin string18 in a==a
        True
        (32.14 secs, 8,048,460,088 bytes)
        *Main> let a = lzwEncode arbreChar string18 in a==a
        True
        (12.12 secs, 4,300,215,536 bytes)

        *Main> let a = lzwEncode charsMin string20 in a==a
        True
        (244.45 secs, 61,103,861,384 bytes)
        *Main> let a = lzwEncode arbreChar string20 in a==a
        True
        (56.08 secs, 19,658,114,232 bytes)

D'apres ces deux tests on remarque que la structure des arbres permet un encodage plus rapide que celle des listes.

    arbreDico :: Apref 
    arbreDico = foldl ajouter (empty::Apref) $ map (\c -> [c]) (['\NUL'..'\255'])

    dicobis :: ListeAssociative 
    dicobis = foldl ajouter (empty::ListeAssociative) $ map (\c -> [c]) (['\NUL'..'\255'])

    deepcheck p = quickCheckWith stdArgs { maxSuccess = 20 ,maxSize = 1000} p

    testGlobal1 = deepcheck ((\s -> (lzwDecode arbreDico (lzwEncode arbreDico s)) == s) :: [Char] -> Bool)
    testGlobal2 = deepcheck ((\s -> (lzwDecode dicobis (lzwEncode dicobis s)) == s) :: [Char] -> Bool)

Et du coup si on veut faire les 20 tests sur la validite de la compression et la decompression avec quickCheck on aura le resultat suivant :

    *Main> testGlobal1
    +++ OK, passed 20 tests.
    *Main> testGlobal2
    +++ OK, passed 20 tests.

## Tests

**Remarques**A partir du `test18` sa commence a etre tres long pour la liste associative.
Voici des tests des 2 types, nous pouvons voir que l'arbre est plus rapide que la liste associative sur les grandes valeurs

Pour les testes nous avons definis deux tables (charsMin et arbreChar) contenant l'alphabet avec leur code, ainsi que des string aleatoire.
Le dictionnaire de l'arbre a un grand effet sur sa durée d'execution sur des petites valeurs.
Tout les temps d'execution suivant les fonctions seront detailler dans un autre fichier.

		*Main> test16 charsMin
		True
		(6.44 secs, 1,701,083,600 bytes)
		*Main> test16 arbreChar
		True
		(8.77 secs, 2,339,990,280 bytes)
        
		*Main> test18 charsMin
		True
		(43.30 secs, 10,944,666,736 bytes)
        *Main> test18 arbreChar
		True
		(41.08 secs, 11,055,779,552 bytes)
        
        *Main> test20 arbreChar
        True
        (143.97 secs, 48,084,181,176 bytes)
        *Main> test20 charsMin
        True
        (254.38 secs, 63,607,594,816 bytes)
        
Apres examination du temps d'execution :
Arbre :
75% du temps d'execution se fait lors de la recherche du nouveau code dans la fonction ajouter
Resolution : ajouter une variable dans le type de l'arbre qui dit le nombre de noeuds .
16% du temps d'execution vien de StringOf
5% CodeOf

Liste associative :
80% du temps d'execution vien des lookups

Conclusion :
Le simple ajout d'une variable dans arbre pour l'insertion d'une variable permet de reduire son temps d'execution de 75%


## Execution

Pour finir les commandes d'execution pour les fichiers `lzwCompress` et `lzwDecompress` sont :

    $ ./lzwCompress < texte.txt >> code.txt
    $ ./lzwDecompress < code.txt >> texte.txt
