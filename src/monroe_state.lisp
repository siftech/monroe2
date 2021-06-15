(in-package :monroe)
;; this file is a description which allows the generation of the
;; start state
;; each individual goal must
;; also generate some predicates of its own (a wreck for example)
;; there are individual functions to do this below of the form
;; gen-state-<schema-name> which take the schema name and
;; return a 2-tuple (goal state) where goal is the instantiated
;; goal and state is the part of the start state specific to that goal

(defparameter *fixed-state*
  ;; this is part of start state which will *always* be the same in every
  ;; start state (although it may change in the plan) *and* do not
  ;; need to be assigned a location (or the location is fixed as well)
  '(
   ;;;;;;; locations

   ;; service stations
   (service-station texaco1)
   (in-town texaco1 brighton)

   ;; hospitals
   (hospital strong)
   (in-town strong rochester)

   (hospital park-ridge)
   (in-town park-ridge greece)

   (hospital rochester-general)
   (in-town rochester-general rochester)
   (hospital-doesnt-treat rochester-general broken-leg)

   ;; dumps
   (garbage-dump brighton-dump)
   (in-town brighton-dump brighton)

   (garbage-dump henrietta-dump)
   (in-town henrietta-dump henrietta)

   ;; malls
   (mall marketplace)
   (in-town marketplace henrietta)

   ;; transport-hubs
   (transport-hub airport)
   (in-town airport gates)

   ;; schools
   (school brighton-high)
   (in-town brighton-high brighton)

   ;; parks
   (park mendon-pond)
   (in-town mendon-pond mendon)

   ;; general points
   (point 12-corners)
   (in-town 12-corners brighton)

   (point pittsford-plaza)
   (in-town pittsford-plaza pittsford)

   ;; just assume everywhere is a town (this combines town/villages web/pitts
   (town rochester)
   (town brighton)
   (town mendon)
   (town hamlin)
   (town webster)
   (town irondequoit)
   (town henrietta)
   (town greece)
   (town parma)
   (town clarkson)
   (town sweeden)
   (town ogden)
   (town gates)
   (town riga)
   (town chili)
   (town wheatland)
   (town pittsford)
   (town scottsville)
   (town rush)
   (town perinton)
   (town fairport)
   (town penfield)
   (town east-rochester)
   (town churchville)
   (town brockport)
   (town spencerport)
   (town hilton)
   (town honeoye-falls)

   ;; power
   (powerco-of rochester rge)
   (powerco-of gates rge)
   (powerco-of brighton rge)
   (powerco-of henrietta rge)
   (powerco-of greece rge)
   (powerco-of chili rge)
   (powerco-of mendon mendon-ge)
   (powerco-of hamlin monroe-ge)
   (powerco-of webster monroe-ge)
   (powerco-of irondequoit monroe-ge)
   (powerco-of parma monroe-ge)
   (powerco-of clarkson monroe-ge)
   (powerco-of sweeden monroe-ge)
   (powerco-of ogden monroe-ge)
   (powerco-of riga monroe-ge)
   (powerco-of wheatland monroe-ge)
   (powerco-of pittsford monroe-ge)
   (powerco-of scottsville monroe-ge)
   (powerco-of rush monroe-ge)
   (powerco-of perinton monroe-ge)
   (powerco-of fairport monroe-ge)
   (powerco-of penfield monroe-ge)
   (powerco-of east-rochester monroe-ge)
   (powerco-of churchville monroe-ge)
   (powerco-of brockport monroe-ge)
   (powerco-of spencerport monroe-ge)
   (powerco-of hilton monroe-ge)
   (powerco-of honeoye-falls monroe-ge)

   ;; water
   (waterco-of rochester roch-water)
   (waterco-of gates roch-water)
   (waterco-of brighton roch-water)
   (waterco-of henrietta roch-water)
   (waterco-of greece monroe-water)
   (waterco-of chili roch-water)
   (waterco-of mendon mendon-water)
   (waterco-of hamlin roch-water)
   (waterco-of webster monroe-water)
   (waterco-of irondequoit monroe-water)
   (waterco-of parma monroe-water)
   (waterco-of clarkson monroe-water)
   (waterco-of sweeden monroe-water)
   (waterco-of ogden monroe-water)
   (waterco-of riga monroe-water)
   (waterco-of wheatland monroe-water)
   (waterco-of pittsford monroe-water)
   (waterco-of scottsville monroe-water)
   (waterco-of rush monroe-water)
   (waterco-of perinton monroe-water)
   (waterco-of fairport monroe-water)
   (waterco-of penfield monroe-water)
   (waterco-of east-rochester monroe-water)
   (waterco-of churchville monroe-water)
   (waterco-of brockport monroe-water)
   (waterco-of spencerport monroe-water)
   (waterco-of hilton monroe-water)
   (waterco-of honeoye-falls monroe-water)

  ;; serious conditions
   (serious-condition heart-attack)
   (serious-condition head-injury)
   (serious-condition broken-leg)

   )) ;; end fixed-state

(defparameter *fixed-state-need-loc*
  ;; these are fixed things as well, *but* they need to be assigned
  ;; a random location.  Note: this is a list of lists of predicates
  ;; instead of just a list of predicates.  Every predicate in an
  ;; inner list is guaranteed to be assigned the *same* location
  ;; as all the other things in that list.  This is because certain
  ;; things (like a driver and his truck) need to be colocated
  ;; in the start state
  ;; this list assumes that the definition is a 1-place predicate that
  ;; is basically a type predicate
  '(

   ;; people and vehicles
   ((emt-crew emt1) (ambulance amb1))
   ((bus-driver bdriver1) (bus bus1))
   ((tow-truck-driver ttdriver1)(tow-truck ttruck1))
   ((power-crew pcrew1)(power-van van1))
   ((tree-crew tcrew1)(tree-truck ttruck1))
   ((water-crew wcrew1)(water-truck wtruck1))
   ((truck-driver tdriver1)(dump-truck dtruck1))
   ((police-unit pu1)(police-van pvan1))
   ((police-unit pu2)(police-van pvan2))
   ((construction-crew ccrew1)(backhoe backhoe1))
   ((plowdriver pdriver1)) ;; plowdrivers don't need to be with their plows
   ((plowdriver pdriver2))
   ((snowplow plow1))
   ((snowplow plow2))
   ((hazard-team ht1))
   ((hazard-team ht2))
   ((shelter-leader sleader1))
   ((shelter-leader sleader2))
   ((shelter-leader sleader3))

   ;;;;;;;;;;; other stuff
   ;; generators
   ((generator gen1))
   ((generator gen2))
   ;; food - for shelters
   ((food food1))
   ((food food2))
   ((food food3))

   )) ;; end fixed-state-need-loc
  
(defparameter *possible-locs*
  ;; this is a list of locations to use in generating random locations
  ;; for objects
  ;; for now, I think we can just do the generation uniformly
  '(texaco1 strong park-ridge rochester-general marketplace
    airport brighton-high mendon-pond 12-corners pittsford-plaza
    brighton-dump henrietta-dump)
  ) ;; end possible-locs

;;;; supporting code for the gen-states ;;;;;;;;;;;;;;

(defun choose-loc (&optional (fromlist *possible-locs*))
  ;; randomly chooses a location from *possible-locs* or some other list
  (list-choose-uniform fromlist))

(defun choose-road ()
  ;; randomly chooses a from and a to (representing a road)
  ;; and guarantees that they are not the same
  ;; returns them in a 2-tuple (from to)
  (let* ((from (choose-loc))
        (to (choose-loc (remove-first from *possible-locs*))))
    `(,from ,to)))

;;;;;; gen state code for each of the schemas ;;;;;;;;;;;;;;;

(defun gen-state-set-up-shelter (schema)
  ;; need to choose a loc for the shelter
  ;; anything else??
  (let ((loc (choose-loc)))
    (list `(,schema ,loc) ;; goal
          '() ;; state - no state
          )))

(defun gen-state-fix-water-main (schema)
  ;; from and to for street for main
  ;; anything else??
  (let* ((road (choose-road))
         (from (first road))
         (to (second road)))
    (list `(,schema ,from ,to) ;; goal
          '() ;; state - no state
    )))

(defparameter *road-hazardousness-probs*
  ;; a problist for choosing how hazardous a road hazard is
  '((very-hazardous . 0.1)
    (medium-hazardous . 0.4)
    (normal-hazardous . 0.5)))

(defun gen-state-clear-road-hazard (schema)
  ;; need to generate a road for hazard (from to)
  ;; (hazard-seriousness from to 'very-hazardous/normal)
  (let* ((road (choose-road))
         (from (first road))
         (to (second road))
         (hazardousness 
          (problist-choose-stochastic *road-hazardousness-probs*)))
    (list `(,schema ,from ,to) ;; goal
          ;; state
          `((hazard-seriousness ,from ,to ,hazardousness)))))


(defun gen-state-clear-road-wreck (schema)
  ;; need a from and to for wreck loc
  ;; (wrecked-vehicle from to ?veh) (vehicle ?veh) (atloc ?veh ?from)
  (let* ((road (choose-road))
         (from (first road))
         (to (second road))
         (veh (gen-object 'vehicle)))
    (list `(,schema ,from ,to) ;; goal
          ;; state
          `((vehicle ,veh) (atloc ,veh ,from)
            (wrecked-vehicle ,from ,to ,veh)))))

(defun gen-state-clear-road-tree (schema)
  ;; location for road
  ;; need to generate a tree (tree ?tree)
  ;; (tree-blocking-road ?from ?to ?tree)
  (let* ((tree (gen-object 'tree))
         (road (choose-road))
         (from (first road))
         (to (second road)))
    (list `(,schema ,from ,to) ;; goal
          ;; state
          `((tree ,tree) (atloc ,tree ,from)
            (tree-blocking-road ,from ,to ,tree)))))

(defun gen-state-plow-road (schema)
  ;; need to generate from and to for road to plow
  ;; need (road-snowy from to)
  (let* ((road (choose-road))
         (from (first road))
         (to (second road)))
    (list `(,schema ,from ,to) ;; goal
          `((road-snowy ,from ,to)))))

(defun gen-state-quell-riot (schema)
  ;; need loc of riot
  ;; nothing else?
  (let ((loc (choose-loc)))
    (list `(,schema ,loc)
          '())))

(defun gen-state-provide-temp-heat (schema)
  ;; generate person
  ;; (person person) (atloc person loc)
  (let* ((person (gen-object 'person))
         (loc (choose-loc)))
    (list `(,schema ,person) ;; goal
          ;; state
          `((person ,person)
            (atloc ,person ,loc)))))

(defparameter *power-line-tree-probs*
  ;; a problist for choosing whether or not the downed power line was
  ;; caused by a tree
  '((t . 0.3)
    (() . 0.7)))

(defun choose-power-line-tree ()
  ;; returns true if we decide to make this caused by a tree
  (problist-choose-stochastic *power-line-tree-probs*))

(defun gen-state-fix-power-line (schema)
  ;; need loc for line
  ;; also, decide if we want the downed line to be b/c of a tree
  ;; if tree, then need (tree tree) (atloc tree loc)
  ;; otherwise, shouldn't need anything
  (let* ((loc (choose-loc))
         (treestate))
    (progn
      (if (choose-power-line-tree)
          (let ((tree (gen-object 'tree)))
            (setf treestate
                  `((tree ,tree)
                    (atloc ,tree ,loc)))))
      (list `(,schema ,loc) ;; goal
            treestate))))

(defparameter *conditions-probs*
  ;; a problist for choosing which condition a patient has
  '((heart-attack . 0.2)
    (broken-leg . 0.1)
    (head-injury . 0.3)
    (bruises . 0.3)
    (light-burn . 0.1)))

(defun choose-condition ()
  ;; chooses a condition for the person
  (problist-choose-stochastic *conditions-probs*))

(defun gen-state-provide-medical-attention (schema)
  ;; generate a person
  ;; (person person) (atloc person loc)
  ;; and a has-condition for them
  (let* ((person (gen-object 'person))
         (loc (choose-loc))
         (condition (choose-condition)))
    (list `(,schema ,person) ;; goal
          `((person ,person)
            (atloc ,person ,loc)
            (has-condition ,person ,condition)))))
