(defpackage :monroe-asd
  (:use :common-lisp :asdf)
  )
(in-package :monroe-asd)
(defsystem monroe
  :depends-on ("shop3")
  :license "BSD 3-clause"
  :serial t
  :version "3.1"
  :components ((:file "package")
               (:file "nlib") ;; my library
               (:file "config")
               ;; (:file "shop2random") ;; planner
               (:file "planlib")      ;; code for generating plan lib
               (:file "monroe_plib")  ;; plan library
               (:file "monroe_state") ;; state
               (:file "create-corpus-code")
               (:file "monroe-config")  ; requires some of the
                                        ; generation functions in
                                        ; create-corpus-code
               ))
