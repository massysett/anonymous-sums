Data/Sums.hs : generate-sums.hs
	runghc generate-sums.hs Data.Sums 15 > Data/Sums.hs

clean :
	rm Data/Sums.hs
	runhaskell Setup clean

dist : Data/Sums.hs
	runhaskell Setup configure 
	runhaskell Setup sdist

test : Data/Sums.hs
	runghc sunlight-test.hs

.PHONY: clean
