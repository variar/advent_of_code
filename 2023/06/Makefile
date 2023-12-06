hs_input = $(wildcard *.hs)
hs_output := $(hs_input:.hs=.out)

%.out: %.hs
	ghc $< -o $@ 

all: $(hs_output)

clean:
	rm -rf *.out
	