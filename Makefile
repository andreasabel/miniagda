# Makefile for miniagda

files=Tokens Lexer Parser Abstract ScopeChecker SizeChecker TypeChecker Value Signature Termination TermCheck1 TermCheck2
hsfiles=$(foreach file,$(files),$(file).hs)
ghcflags=

default : Main

Main : Main.hs $(hsfiles)
	ghc $(ghcflags) $< --make -o $@

Lexer.hs : Lexer.x
	alex $<

Parser.hs : Parser.y
	happy $<


# EOF
