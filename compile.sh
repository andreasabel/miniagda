#!/bin/sh
alex Lexer.x
happy Parser.y
ghc Main.hs --make -o Main
