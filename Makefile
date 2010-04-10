

SRCFILES=$(shell find src/ -type f)

test: dist/build/testHDF5/testHDF5
	@./Setup.lhs test

dist/build/testHDF5/testHDF5: $(SRCFILES) HDF5hs.cabal Setup.lhs
	@./Setup.lhs configure --user
	@./Setup.lhs build

all:
	./Setup.lhs clean
	./Setup.lhs configure --user
	./Setup.lhs build
	./Setup.lhs install
	./Setup.lhs test
	@mkdir -p doc
	@haddock -o doc --html  `find src/ -name '*.hs' | grep -v '#'` `find dist/build/ -name '*.hs'` --optghc=-XGeneralizedNewtypeDeriving > /dev/null



