
(asdf:defsystem #:cl-prime-maker
  :description "A simple package to generate big prime numbers."
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

