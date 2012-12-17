
(asdf:defsystem #:cl-prime-maker
  :description "
A simple library to generate big prime numbers in a fast way. But in some cases, the generated number is not a prime number (these are called pseudo-primes).
\"The probability of mis-classifying a number is approximately 2^-100. So we can be fairly sure that the classification is correct.\"
"
  :version "0.1"
  :author "Xiaofeng Yang <n.akr.akiiya@gmail.com>"
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
    ((:file "prime-maker"))))
  :depends-on ())

