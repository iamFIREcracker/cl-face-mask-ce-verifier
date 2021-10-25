(asdf:defsystem #:swank-tunnel
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:ngrok
                 #:swank
                 #:uiop
              )

  :components ((:file "main")))
