cl-prime-maker
=============================

A simple library to generate big prime numbers in a fast way. But in some cases, the generated number is not a prime number (these are called pseudo-primes). 

Translated from the erlang version: http://www.oschina.net/code/snippet_222150_8518

##About pseudo-primes##
"The probability of mis-classifying a number is approximately 2^-100. So we can be fairly sure that the classification is correct."

##Usage##

* Load the library

```cl
CL-USER> (ql:quickload "cl-prime-maker")
To load "cl-prime-maker":
  Load 1 ASDF system:
    cl-prime-maker
```

* [cl-prime-maker:make-prime]
 cl-prime-maker:make-prime
 Generates a random prime P with at least K decimal digits. Returns nil when k <= 0. Returns NIL otherwise. K should be an INTEGER. 

```cl
CL-USER> (cl-prime-maker:make-prime 10)
1028450429
CL-USER> (cl-prime-maker:make-prime 10)
247158671
CL-USER> (cl-prime-maker:make-prime 10)
9424855123
CL-USER> (cl-prime-maker:make-prime 100)
2527793987464535166219814069528290578410091106736510171938329845710426162526052832327367116801544019
CL-USER> (time (cl-prime-maker:make-prime 100))
(CL-PRIME-MAKER:MAKE-PRIME 100)
took 516 milliseconds (0.516 seconds) to run.
During that period, and with 2 available CPU cores,
     516 milliseconds (0.516 seconds) were spent in user mode
       0 milliseconds (0.000 seconds) were spent in system mode
 11,720,160 bytes of memory allocated.
5699885229276577728495724707769425629156908217502336077240701491905327286488809030648850373069454909
```

* [cl-prime-maker:primep]
 Tests if N is a prime number. Returns T if N is a prime number. Returns NIL otherwise. 

```cl
CL-USER> (time (cl-prime-maker:primep 5699885229276577728495724707769425629156908217502336077240701491905327286488809030648850373069454909))
(CL-PRIME-MAKER:PRIMEP 5699885229276577728495724707769425629156908217502336077240701491905327286488809030648850373069454909)
took 390 milliseconds (0.390 seconds) to run.
During that period, and with 2 available CPU cores,
     391 milliseconds (0.391 seconds) were spent in user mode
       0 milliseconds (0.000 seconds) were spent in system mode
 8,757,192 bytes of memory allocated.
T
CL-USER> (time (cl-prime-maker:primep 569988522927657772849572470776942562915690821750233607724070149190532728648880903064885037306945490))
(CL-PRIME-MAKER:PRIMEP 569988522927657772849572470776942562915690821750233607724070149190532728648880903064885037306945490)
took 0 milliseconds (0.000 seconds) to run.
During that period, and with 2 available CPU cores,
     0 milliseconds (0.000 seconds) were spent in user mode
     0 milliseconds (0.000 seconds) were spent in system mode
 89,992 bytes of memory allocated.
NIL


```