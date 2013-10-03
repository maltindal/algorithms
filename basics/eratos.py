class Primes:
    
    # compute primes up to the number n where n > 2
    def sieve(self, n):
        if n < 2:
            return []
        else:
            ns = range(2,n+1)
            r = list(ns) # copy list ns
            for n in ns:
                for x in range(n+n, ns[-1]+1, n):
                    r[x-2] = 0 # assumes that the list start with 2

            return [n for n in r if n != 0]

if __name__ == "__main__":
    p = Primes()

    print "Sieve primes 2..1000"
    print p.sieve(1000)

