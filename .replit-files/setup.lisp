(load "init.lisp")

(pushnew '(merge-pathnames (parse-namestring ".replit-files/swank-tunnel")
           *default-pathname-defaults*)
         asdf:*central-registry*)
(pushnew '(merge-pathnames (parse-namestring ".replit-files/ngrok")
           *default-pathname-defaults*)
         asdf:*central-registry*)
