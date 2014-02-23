(asdf:defsystem #:cl-prime-maker
  :description "
A simple library to generate big prime numbers in a fast way. But in some cases, the generated number is not a prime number (these are called pseudo-primes).
\"The probability of mis-classifying a number is approximately 2^-100. So we can be fairly sure that the classification is correct.\"
"
  :version "0.2"
  :author "Xiaofeng Yang <n.akr.akiiya at gmail.com>"
  :license "BSD"
  :serial t
  :components
  ((:module
    "package-init"
    :pathname #P"src/"
    :components
    ((:file "packages")))
   (:module
    "sources"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "prime-maker")
     (:file "sm-ruiz-2000"))))
  :depends-on ())

