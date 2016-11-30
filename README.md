## Introduction

At the top of the file there should be a short introduction and/ or overview that explains **what** the project is. This description should match descriptions added for package managers (Gemspec, package.json, etc.)

## Motivation

Show what the library does as concisely as possible, developers should be able to figure out **how** your project solves their problem by looking at the code example. Make sure the API you are showing off is obvious, and that your code is short and concise.

## Un peu de Code

A short description of the motivation behind the creation and maintenance of the project. This should explain **why** the project exists.

## Execution

Provide code examples and explanations of how to get the project.

## Testes

Depending on the size of the project, if it is small and simple enough the reference docs can be added to the README. For medium size to larger projects it is important to at least provide a link to where the API reference docs live.



    dico :: ListeAssociative
    dico = L [(i,[(toEnum i :: Char)])|i<-[0..255]]

    mytest = quickCheck ((\s -> (lzwDecode dico (lzwEncode dico s)) == s) :: [Char] -> Bool)
