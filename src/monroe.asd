(defpackage :monroe-asd
  (:use :common-lisp :asdf)
  )
(in-package :monroe-asd)
(defsystem monroe
    ;; need new version of SHOP3 with its own random function
  :depends-on ((version "shop3" "4"))
  :license "BSD 3-clause"
  :serial tg
  :version "4.0.0"
  :components ((:file "package")
               (:file "nlib") ;; my library
               (:file "config")
               (:file "planlib")      ;; code for generating plan lib
               (:file "monroe_plib")  ;; plan library
               (:file "monroe_state") ;; state
               (:file "create-corpus-code")
               (:file "monroe-config")  ; requires some of the
                                        ; generation functions in
                                        ; create-corpus-code
               ))
