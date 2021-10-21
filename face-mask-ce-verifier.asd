(asdf:defsystem #:face-mask-ce-verifier
  :description "A small system to check if a notified body is authorized to certify face masks in the UE"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:binding-arrows
                 #:dexador
                 #:cl-ppcre
                 #:lquery
                 #:plump
                 #:split-sequence
              )

  :components ((:file "src/main")))
