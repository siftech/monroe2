(in-package :monroe)

(defclass monroe-config ()
  ((common-state
    :initarg :common-state
    :accessor common-state
    )
   (plan-package
    :initarg :plan-package
    :type (or package string symbol)
    )
   (domain-name
    :initarg :domain-name
    :reader domain-name
    ))
  (:documentation "Object encapsulating Monroe configuration so that 
we can override it."))

(defclass monroe-corpus-config (monroe-config)
  ()
  (:default-initargs :plan-package '#:monroe)
  (:documentation "This is the configuration class for Nate's original
  MONROE."))

(defgeneric plan-ok (monroe-config plan)
  (:documentation "Predicate to be called on each plan to decide if
  it's acceptable."))

(defgeneric generate-goal-and-start-state (monroe-config schema)
  (:documentation "Takes a configuration object and a goal schema as
  arguments and returns a list, whose FIRST is a revised task and
  whose SECOND is a start state."))

(defgeneric choose-goal-schema (config))

(defgeneric top-goals (config)
  (:documentation "If using the standard method of choosing a goal
schema, this should return a problist of goal schemas.")
  (:method ((config monroe-config))
    (declare (special *top-goals*) (ignorable config))
    *top-goals*)
  )

(defgeneric get-top-goal-schemas (config)
  (:documentation "Returns a list of the top goals of the domain.
Helper function for GET-GOAL-SCHEMAS."))

(defgeneric get-goal-schemas (config)
  (:documentation "returns a list of the goals of the domain
in case of flat recognition (*FLAT-OUTPUT* is non-NIL), these 
are also the top goals. For hierarchical, they include the top and
subgoals"))

(defgeneric get-method-schemas (config)
  (:documentation "Returns a list of *unique* method schemas from 
the domain so things like GET-TO will just be 1 schema, even though
there are many different method bodies."))

(defmethod plan-package ((obj monroe-config))
  (with-slots (plan-package) obj
    (etypecase plan-package
      (package plan-package)
      ((or symbol string) (uiop:find-package* plan-package)))))

;;; specials
(declaim (special *top-goals* *flat-output*))
(defvar *problem-filename*
  nil
  "Used to dump problems, when desired.  Internal use only: control
  with arguments.")
