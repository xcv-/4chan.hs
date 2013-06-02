all: build

build:
	ghc --make Main.hs

run:
	./Main

clean:
	find . -name '*.o' -delete
	find . -name '*.hi' -delete
	rm Main
