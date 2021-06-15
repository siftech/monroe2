(defpackage monroe
  (:use common-lisp shop2)
  (:export
   ;; generation
   #:gg
   #:cc-main
   #:get-corpus
   
   ;; object to configure plan library, etc. for Monroe domain
   #:monroe-config

   ;;#:defdomain
   ;;#:defproblem

   ;; to set up a set of top goals
   #:freqlist-to-problist
   #:problist-choose-stochastic

   ;; dynamic variable to configure output

   ;; generic methods
   #:generate-goal-and-start-state
   #:choose-goal-schema
   #:top-goals
   #:get-top-goal-schemas
   #:get-goal-schemas
   #:get-method-schemas
   #:plan-ok
   ;; config accessors
   #:common-state
   #:plan-package
   ))