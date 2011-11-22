
##############################################################################
## Benchmarks
##############################################################################

## Config
#########

GHC6 = ghc-6.12.3
GHC7 = ghc-7.0.3

GHC = $(GHC7)

GHCI = ghci-6.12.3


## All benchmarks
#################

# clean-bench-all:
# 	rm -f *.o *.hi Attoget

## Individual benchmarks
########################

# utf8 writing to a file
bench-attoget:
	$(GHC) --make -O2 -fforce-recomp -main-is Attoget Attoget.hs
	./Attoget --resamples 10000

