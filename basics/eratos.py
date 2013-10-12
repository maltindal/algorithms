import matplotlib.pyplot as plt

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

    # given a sequence of prime numbers calculate the distance
    # between two prime numbers next to each other
    def distances(self, ps):
        return [ps[i+1]-ps[i] for i in range(0, len(ps)-1, 2)]

    def plot(self, n):
        plt.scatter(range(len(n)), n)
        plt.show()

if __name__ == "__main__":
    p = Primes()

    n = 10000000
    ps = p.sieve(n)

    print "Sieve primes 2..%s" % (n)
    print ps

    print()
    print "Plotting primes"
    p.plot(ps)
