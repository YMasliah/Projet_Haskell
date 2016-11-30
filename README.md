## Introduction

Le but du projet est d'implementer l'algorithme de compression et de decompression de **Lempel-Ziv-Welch** avec differentes structures de donnes comme table de traduction, qui associe des chaines de caracetre a des codes (dans notre cas des entiers).
Les deux structures que nous avons implementer sont les listes associatives et les arbres de prefixes, nous avons pas eu le temps pour l'instant de finir la partie avec les bits.

## Structuration du Projet

          \Projet_Haskell
                    Main_WO_Bits.hs
                    README.md
                    lzwDecode
                    lzwEncode
Un fichier `README.md` pour le rendu du projet ainsi que les etapes a suivre pour executer et tester les fonctionnalitees, les deux fichier executables `lzwEnCode` et `lzwDecode` sert a coder ou decoder un fichier.
Et enfin le fichier principale `Main_WO_Bits.hs` qui contient les focntions et les ou on pourra tester notre travail.

## Un peu de Code

         empty :: a 
         ajouter :: a -> String -> a
         codeOf :: a -> String -> Maybe Code
         stringOf :: a -> Code -> Maybe String
         isIn :: a -> String -> Bool
         split :: a -> String -> (String,Maybe Code,String)

## Execution

Provide code examples and explanations of how to get the project.

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
