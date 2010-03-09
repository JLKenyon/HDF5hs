
simple:
	./Setup.lhs build
	./Setup.lhs test

all:
	./Setup.lhs clean
	./Setup.lhs configure --user
	./Setup.lhs build
	./Setup.lhs install
	./Setup.lhs test

