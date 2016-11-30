## Introduction

At the top of the file there should be a short introduction and/ or overview that explains **what** the project is. This description should match descriptions added for package managers (Gemspec, package.json, etc.)

## Motivation

Show what the library does as concisely as possible, developers should be able to figure out **how** your project solves their problem by looking at the code example. Make sure the API you are showing off is obvious, and that your code is short and concise.

## Un peu de Code

A short description of the motivation behind the creation and maintenance of the project. This should explain **why** the project exists.

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
