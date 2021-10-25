(asdf:defsystem #:face-mask-ce-verifier-web
  :description "A Web UI for the #:face-mask-ce-verifier system."

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:face-mask-ce-verifier
                 #:lass
                 #:hunchentoot
                 #:spinneret
                 #:uiop
              )

  :components ((:file "src/web")))
