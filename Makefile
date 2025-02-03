default:
	stack build

doc:
	${BLDTOOL} haddock --open

format:
	stylish-haskell  -c .stylish-haskell.yaml  -i  -r src/

clean:
	stack clean

