(in-package :monroe)
;; code for generating the plan library file

;; (defun lplib ()
;;   ;; loads this
;;   (load "planlib"))

(defmethod get-top-goal-schemas ((obj monroe-config))
  ;; returns a list of the top goals of the domain
  ;; they are found in the variable *top-goals*
  (mapcar #'(lambda (val)
              (car val))
          *top-goals*))

(defmethod get-goal-schemas ((obj monroe-config))
  ;; returns a list of the goals of the domain
  ;; in case of flat recognition, these are also the top goals
  ;; for hierarchical, they include the top and subgoals
  (if *flat-output*
      (get-top-goal-schemas obj)
    (get-method-schemas obj)))

(defmethod get-method-schemas ((obj monroe-config))
  ;; returns a list of *unique* method schemas from the domain
  ;; so things like get-to will just be 1 schema, even though there
  ;; are many different method bodies

  (let ((mhash (make-hash-table)))
    ;; make unique list to return
    (mapcar #'(lambda (method)
                (let ((name (schema-name method)))
                  (setf (gethash name mhash) method)))
            ;; get a list of all methods (including duplicates)
            (mymaphash #'(lambda (key val)
                           (declare (ignore key))
                           (method-schema val))
                       (shop2::domain-methods (find-domain (domain-name obj)))))
    (gethashvals mhash)))

(defun method-schema (method)
  (cadar method))

(defun get-operator-schemas (config)
  ;; returns a list of operator schemas from domain
  (mymaphash #'(lambda (key val)
                 (declare (ignore key))
               (operator-schema val))
             (shop2::domain-operators (find-domain (domain-name config)))))

(defun schema-name (schema)
  ;; returns name of a schema header
  (first schema))

(defun schema-arity (schema)
  ;; returns arity of a schema header
  (length (rest schema)))

(defun operator-schema (operator)
  ;; given an operator, returns its schema header (in form (!op ?arg ?arg))
  (shop2::operator-head operator))

(defun output-max-depth (outfile max-depth)
  ;; outputs a line saying how deep the max depth for this
  ;; library is
  (format outfile "Max Depth: ~d~%~%" max-depth))

(defun output-top-goals (config-obj outfile)
  ;; outputs a list of the top-level goals of the domain, as defined
  ;; in the <domain>_plib.lisp file as *top-goals*
  (format outfile "Top-level Goals:~%")
  (mapcar (lambda (val)
            (format outfile
                    "~A~%"
                    (act-to-string val)))
          (get-top-goal-schemas config-obj)))

(defun output-subgoals (config-obj outfile)
  ;; outputs all subgoals.  this includes top-goals as well, as they
  ;; could potentially be subgoals
  (format outfile "~%Subgoals:~%")
  (mapcar (lambda (val)
            (format outfile
                    "~A~%"
                    (act-to-string val)))
          (get-goal-schemas config-obj)))

(defun output-atomic-actions (config outfile)
  (format outfile "~%Atomic Actions:~%")
  (mapcar (lambda (val)
            ;; FIXME: probably should remove *all* internal operators.
            (if (not (eq (car val) '!!inop)) ;; don't include internal !!INOP
                (format outfile
                        "~A~%"
                        (act-to-string val))))
          (get-operator-schemas config)))

(defun output-common-state (config stream)
  (format stream "~%Common problem state:")
  (let ((fixed-state (sort (copy-tree (common-state config)) 'prop-compare)))
    (let ((*print-readably* t)
          (*package* (plan-package config)))
      (mapc #'(lambda (prop) (print prop stream)) fixed-state))))

(defun prop-compare (prop1 prop2)
  (labels ((compare-next (l1 l2)
             (cond ((null l1)
                    ;; if L1 is shorter, it's first
                    (not (null l2)))
                   ((null l2) nil)
                   (t
                    (let ((elem1 (first l1))
                          (elem2 (first l2)))
                      (cond ((listp elem2)
                             (if (listp elem1)
                                 (compare-next elem1 elem2)
                                 nil))
                            ;; neither is a list (function), so both atoms
                            (t (let ((se1 (string elem1))
                                     (se2 (string elem2)))
                                 (cond
                                   ((string-lessp se1 se2)
                                    t)
                                   ((string-lessp se2 se1)
                                    nil)
                                   (t (compare-next (rest l1) (rest l2))))))))))))
    (compare-next prop1 prop2)))


(defun output-planlib (config-obj plib-filename max-depth)
  ;; main function: writes out a planlib file
  ;; which can be compiled into a plan library for perl
  (with-open-file (outfile plib-filename :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
    (output-max-depth outfile max-depth)
    (output-top-goals config-obj outfile)
    (output-subgoals config-obj outfile)
    (output-atomic-actions config-obj outfile)
    (output-common-state config-obj outfile)))
