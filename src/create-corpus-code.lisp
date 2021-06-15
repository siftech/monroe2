(in-package :monroe)
;; runs planner and outputs a plan corpus

;;;;;;;;;;;;;;;;;;;;;;;;;; loads ;;;;;;;;;;;;;;;;;;;
;; (load "nlib") ;; my library
;; (load "shop2random") ;; planner
;; (load "planlib") ;; code for generating plan lib
;; (load "monroe_plib") ;; plan library
;; (load "monroe_state") ;; state

;;;;;;;;;;;;;;;;;;;;;;;;;;;; shortcuts ;;;;;;;;;;;;;;;
;; (defun lcc () ;; loads this file
;;   (load "create-corpus-code"))

(declaim (special *monroe*))            ;defined in monroe-config

(defun gg (size &key flat (object *monroe*) problem-file) ;; this one is for interactive calling
  (cc-main "plib.txt" "corpus.txt" size object flat problem-file))

;;;;;;;;;;;;;;;;;;;;;;;;; global variables ;;;;;;;;;;;;;;;;;;;;
(defvar *flat-output* '()) ;; whether or not we're outputing for flat or hierarchical

(defparameter *corpus-max-depth* -1) ;; this keeps track of the maximum plan
;; tree depth for any of the sessions in the corpus, so we can
;; output it in the planlib and then use it in hierarchical recognition
;; to get around any recursive methods.  It's a hack, but not
;; too ugly a one - we could always constrain our planner to not
;; output plans > max-depth (if we were doing it in a more principled
;; way

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;

(defun extract-acts (plan)
  (every-other plan 1 0))

(defun munge (plan)
  (extract-acts (car plan)))

(defun get-goal (prob)
  (cdr (get-tasks prob)))

(defun get-schema (act)
  ;; returns the schema part of the action as an atom
  (car act))

(defun get-params (act)
  ;; returns the params part as a lst
  (cdr act))

(defun params-to-string (params)
  ;; turns a param list into a string (with commas)
  (if (endp params)
      ""
      (if (eq 1 (length params))
          (format nil "~A" (car params))
          ;; else longer than 1
          (format nil "~A,~A" 
                  (car params)
                  (params-to-string (cdr params))))))

(defun act-to-string (act)
  ;; turns the action into a string
  (format nil "~A(~A)" 
          (get-schema act) 
          (params-to-string (get-params act))))

(defun output-session-flat (goal plan outfile)
  ;; prints out this particular plan session
  (format outfile "~%Goal: ~A~%" (act-to-string goal))
  (mapc #'(lambda (act)
            (format outfile "  ~A~%" (act-to-string act)))
        plan))


(defmethod choose-goal-schema ((obj monroe-config))
  ;; chooses the goal stochastically
  (problist-choose-stochastic *top-goals*))

(defun create-atloc (objname loc)
  ;; creates the form (atloc objname loc)
  `(atloc ,objname ,loc))

(defun gen-loc-pred (pred loc)
  ;; for a single predicate, returns a list of itself and its corresponding
  ;; atloc
  (list pred (gen-loc-obj (second pred) loc)))

(defun gen-loc-obj (obj loc)
  ;; for a single object, generate location
  (create-atloc obj loc))

(defun gen-fixed-loc (loclist possible-locs)
  ;; generates a location for a single list of predicates
  ;; and returns a list of those predicates and corresponding
  ;; atlocs
  (labels
      ((gather-list (loclist loc)
         ;; goes through each element of list and adds the atloc to it
         (if (endp loclist)
             '()
             (append (gather-list (cdr loclist) loc)
                     (gen-loc-pred (car loclist) loc)))))
    (let ((loc (list-choose-uniform possible-locs)))
      (gather-list loclist loc))))


(defun gen-fixed-locs (list possible-locs)
  ;; generates the fixed-state-need-loc part of the state
  ;; list is a list of lists of predicates, each list representing
  ;; an equivalence set of items that need to be colocated
  ;; this returns a list of those predicates as well as their
  ;; corresponding atloc predicates
  (if (endp list)
      '()
      (append (gen-fixed-loc (car list) possible-locs)
              (gen-fixed-locs (cdr list) possible-locs))))

(defun gen-fixed-start-state (fixed-state fixed-state-need-loc possible-locs)
  ;; generates the fixed part of the start state (everything but the
  ;; goal specific stuff
  (append fixed-state
          (gen-fixed-locs fixed-state-need-loc possible-locs)))

(defmethod common-state :before ((conf monroe-corpus-config))
  (unless (slot-boundp conf 'common-state)
    (setf (common-state conf)
          (gen-fixed-start-state *fixed-state*
                                 *fixed-state-need-loc*
                                 *possible-locs*))))  

(defun get-goal-func-name (schema-name)
  ;; this turns a schema name into a function name for that specific
  ;; function of the form gen-state-<schema-name>
  ;; returns this as a string
  (format nil "GEN-STATE-~A" schema-name))

(defun gen-object (&optional (type 'obj))
  (gensym (format nil "~A-" type)))

(defun gen-goal-and-state (schema)
  ;; generates the goal and the non-fixed part of the start state
  ;; i.e. the part that is specific to this goal schema
  ;; returns in form (goal state)
  ;; this function basically calls specific functions to the schema
  (let* ((schema-name (get-schema schema))
         (func-name (get-goal-func-name schema-name)))
    ;; this creates a function name based on the schema and calls it
    (apply (intern func-name :monroe) (list schema-name))))

(defmethod generate-goal-and-start-state ((config-obj monroe-config) schema)
  ;; generates the goal and start state given the schema
  ;; it is easiest to just generate both together, since specific
  ;; code needs to be called for each goal schema
  ;; returns a 2-tuple of form (goal start-state)
  (let* ((fixed-start-state (common-state config-obj))
         (goal-and-state (gen-goal-and-state schema))
         (goal (first goal-and-state))
         (state (second goal-and-state))
         (start-state (append state fixed-start-state)))
    `(,goal ,start-state)))

(defun generate-problem (config-obj)
  ;; generates the problem (goal and state) to be used to plan with
  ;; doesn't return anything, the problem is stored internally
  ;; in the planner as 'generated-problem
  (let* ((schema (choose-goal-schema config-obj))
         (goal-and-start-state (generate-goal-and-start-state config-obj schema))
         (goal (first goal-and-start-state))
         (start-state (second goal-and-start-state)))
    (make-problem 'generated-problem (domain-name config-obj) start-state goal)
    (format t "~%Choosing goal: ~S~%" (get-tasks 'generated-problem)))
  )

(defun atomic-level (action)
  ;; if the action is an atomic-level action
  ;; the output is (1.0 (!act param) num)
  (numberp (car action)))

(defun output-goalchain (outfile goalstack action)
  ;; outputs a single line (i.e., goalchain and action)
  ;; also updates *corpus-max-depth*
  (let ((depth (1+ (length goalstack))))
    (if (> depth *corpus-max-depth*)
        (setq *corpus-max-depth* depth)))
  (format outfile "~A::~A~%"
          (act-to-string action)
          (perljoin ";"
                    (mapcar #'act-to-string goalstack))))


(defun output-plan (config-obj outfile plans)
  (let ((*print-readably* t)
        (*package* (plan-package config-obj)))
    (destructuring-bind (plan-forests plan-sequences) plans
      (let ((plan-forest (first plan-forests))
            (plan-sequence (first plan-sequences)))
        (if *flat-output*
            (output-session-flat
             (get-goal 'generated-problem)
             (shop2:shorter-plan plan-sequence)
             outfile)
            ;; else hierarchical output
            (progn
              ;; ugh -- it's really horrible that the tree ISN'T A BLOODY TREE!
              ;; SHOP2's "trees" are really forests.
              (mapcar #'(lambda (tree) (output-tree tree outfile)) plan-forest)
              (terpri outfile)
              (let ((*print-pretty* t)
                    (*print-readably* t))
                (mapc #'(lambda (act) (print act outfile))
                       (shop2:shorter-plan plan-sequence)))
              (terpri outfile)
              (terpri outfile)))))))

;;; Print a plan tree, and as a side-effect, track the depth of the corpus.
(defun output-tree (tree stream)
  (labels ((traverse (node depth)
             (if (primitive-node-p node)
                 (progn
                   (when (> depth *corpus-max-depth*)
                     (setf *corpus-max-depth* depth))
                   (tree-node-task node))
                 (list (tree-node-task node) (mapcar #'(lambda (n) (traverse n (1+ depth))) (complex-node-children node))))))
    (let ((tree
            (traverse tree 0)))
      (let ((*print-readably* t)
            (*print-pretty* t))
        (print tree stream)))))


(defun osp (plan)
  ;; test function to output a single plan
  (with-open-file (outfile "test.pc" :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
                  (output-plan *monroe* outfile plan)))


(defun get-random-plan (config-obj &optional (which :random))
  ;; returns a random plan
  (format T "~%generating problem")
  (generate-problem config-obj) ;; generates the prob with goal
  (format T "~%beginning to plan..")
  (multiple-value-bind (plans runtime trees)
      (generate-random-plan which)
    (declare (ignore runtime))
    (format T "finished planning~%")
    ;; monroe was based on lists, rather than multiple values...
    (list trees plans)))

(defun generate-random-plan (&optional (which :random))
  (find-plans 'generated-problem 
                          :which which
                          :verbose 0
                          :plan-tree t))
  

(defun num-steps (plan)
  ;; returns the number of immediate children in the plan
  ;; i.e., the next level down (not atomic)
  (length plan))

(defmethod plan-ok ((obj monroe-config) plan)
  (declare (ignorable obj))
  ;; checks a generated plan to make sure we want to keep it
  ;; right now, makes sure plans are at least 2 steps (immediate subgoals) long
  ;; (1-step or 0-step plans aren't interesting to us)
  ;; NOTE: PLAN is a misnomer for this value, which is actually:
  ;; ( (PLAN-TREE) (PLAN-SEQUENCE) )
  ;; also, Nate checks the number of subgoals to the top-goal, rather than the number of  which could underestimate the number of steps in the plan.  I modified the check to agree with the comment, rather than maintaining it's 
  (let ((top-subgoals (complex-node-children (caaar plan))))
    (if (> (num-steps top-subgoals) 2)
        plan
        (progn
          (format t "~%bad plan: ~A~%" plan)
          nil))))

(defun get-acceptable-plan (config-obj)
  ;; returns a random plan that is also acceptable (by plan-ok)
  (LOOP :FOR plan = (get-random-plan config-obj)
        :UNTIL (plan-ok config-obj plan)
        :FINALLY (progn
                   (when *problem-filename*
                     (with-open-file (str *problem-filename*
                                          :direction :output
                                          :if-does-not-exist :create
                                          :if-exists :append)
                       (let ((*print-readably* t)
                             (*package* (plan-package config-obj)))
                         (let ((problem (find-problem 'generated-problem)))
                           (setf (slot-value problem 'shop2::name) (gensym "PROB"))
                           (print problem str)))))
                   (return-from get-acceptable-plan plan))))

(defun get-sessions-single-problem (config-obj outstream)
  ;; creates a single problem and goal and extracts a plan for it
  ;; which it writes to outstream
  (output-plan
   config-obj
   outstream
   (get-acceptable-plan config-obj)))

(defun get-sessions-open-file (config-obj outstream problem-filename corpus-size)
  ;; auxiliary for get-sessions, takes an open file and writes
  ;; the corpus to it, called recursively to loop
  ;; to get each individual case
  (let ((*problem-filename* problem-filename))
    (dotimes (x corpus-size)
      (format t "~%creating session~D~%" (1+ x))
      (get-sessions-single-problem config-obj outstream)
      )))

(defun get-sessions (config-obj filename problem-filename corpus-size)
  (with-open-file (outstream filename :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
      (get-sessions-open-file config-obj outstream problem-filename corpus-size)
      ;; will do this upon reading in into perl plan corpus
      ;;(if (not *flat-output*)
      ;;(format outfile "::Maximum Tree Depth: ~A~%" *corpus-max-depth*)))
      ))

(defun get-corpus (config-obj plib-filename corpus-filename corpus-size problem-filename)
  ;; do corpus stuff
  (get-sessions config-obj corpus-filename problem-filename corpus-size)
  ;; write out plan library
  (output-planlib config-obj plib-filename *corpus-max-depth*)
  (values))

(defun cc-main (plib-filename
                corpus-filename corpus-size
                config-obj
                &optional flat-output problem-file)
  ;; main routine
  ;; args = (string: plibfilename, outputfilename, num: corpus-size,[bool: flat-output])
  (let ((*flat-output* flat-output) ;; set if we are outputting for flat or hierarchical
        (*top-goals* (top-goals config-obj)))
    (when problem-file (uiop:delete-file-if-exists problem-file))
    (get-corpus config-obj plib-filename
                corpus-filename corpus-size
                problem-file)))
