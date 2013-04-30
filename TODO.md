* PrimeQ first tests for divisibility using small primes, then uses the [Miller-Rabin](http://mathworld.wolfram.com/Rabin-MillerStrongPseudoprimeTest.html) strong pseudoprime test base 2 and base 3, and then uses a [Lucas](http://mathworld.wolfram.com/LucasPseudoprime.html) test. 
* As of 1997, this procedure is known to be correct only for n<10^16, and it is conceivable that for larger n it could claim a composite number to be prime. 

Use the same way as Mathematica does to detect the numbers.
- [ ] Tests for divisibility using small primes.
- [ ] Implements Miller-Rabin strong pseudoprime test.
- [ ] Implements Lucas test.