# Makefile for miniagda

files=Tokens Lexer Parser Abstract ScopeChecker TypeChecker Value Termination TermCheck2 SPos
hsfiles=$(foreach file,$(files),$(file).hs)
ghcflags=

default : Main

Main : Main.hs $(hsfiles)
	ghc $(ghcflags) $< --make -o $@

Lexer.hs : Lexer.x
	alex $<

Parser.hs : Parser.y Lexer.hs
	happy $<


# EOF
