(in-package :monroe)
;; general notes about this domain encoding:

;; many things are kludged here, first to make them possible, and
;; second to make them easier

;; in principle, things can either be in a place (point) or on
;; a road.  Roads are described by their endpoints.  I just assume
;; there is (potentially) exactly 1 road between each pair of endpoints
;; so roads are just named by their endpoints.  Also, roads are 1 way
;; so if you switch the endpoints your talking about a totally differnt
;; road.

;; for simplicity of things that happen on roads (like clear-road)
;; I just *do* the activities at the ?from location.

;; this list describes the top-level goal schemas
;; this is a list of lists, where the lists, for now, have
;; the schema header and a number which indicates a priori likelihood
;; basically, these 'numbers' for each goal are added up to SUM
;; and the apriori likelihood of a given goal is number/SUM
;; just an easy way of doing it to make sure the probs always
;; add up to 1
(defparameter *top-goals*
  (freqlist-to-problist ;; converts the numbers to probs
   '(((set-up-shelter ?loc) . 1)
     ((fix-water-main ?from ?to) . 1)
     ((clear-road-hazard ?from ?to) . 3)
     ((clear-road-wreck ?from ?to) . 5)
     ((clear-road-tree ?from ?to) . 2)
     ((plow-road ?from ?to) . 7)
     ((quell-riot ?loc) . 1)
     ((provide-temp-heat ?person) . 3)
     ((fix-power-line ?lineloc) . 2)
     ((provide-medical-attention ?person) . 7)
)))

(defdomain monroe

 (

;;;;;;;;;;;;;;;;;;;;;;;;; main goals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;set-up-shelter sets up a shelter at a certain location
  (:method (set-up-shelter ?loc)
           normal
           ((shelter-leader ?leader)
            (not (assigned-to-shelter ?leader ?_other-shelter))
            (food ?food))
           ((get-electricity ?loc) (get-to ?leader ?loc) (get-to ?food ?loc)))

;;fix-water-main
  (:method (fix-water-main ?from ?to)
           normal
           ()
           ((shut-off-water ?from ?to) 
            (repair-pipe ?from ?to)
            (turn-on-water ?from ?to)))

;; clear-road-hazard - cleans up a hazardous spill
  (:method (clear-road-hazard ?from ?to)
           normal
           ()
           ((block-road ?from ?to)
            (clean-up-hazard ?from ?to)
            (unblock-road ?from ?to)))

;; clear-road-wreck - gets a wreck out of the road
  (:method (clear-road-wreck ?from ?to)
           normal
           ()
           ((set-up-cones ?from ?to)
            (clear-wreck ?from ?to)
            (take-down-cones ?from ?to)))

;; clear-road-tree
  (:method (clear-road-tree ?from ?to) ;; clears a tree that's in the road
           normal
           ((tree-blocking-road ?from ?to ?tree))
           ((set-up-cones ?from ?to)
            (clear-tree ?tree)
            (take-down-cones ?from ?to)))

;; plow-road
  (:method (plow-road ?from ?to)
           plow
           ((road-snowy ?from ?to)
            (snowplow ?plow)
            (atloc ?plow ?plowloc)
            (plowdriver ?driver)
            )
           ((get-to ?driver ?plowloc)
            (!navegate-snowplow ?driver ?plow ?from) ;; must use nav-snowplow
                                ;; since regular cars can't drive if snowy
            (!engage-plow ?driver ?plow)
            (!navegate-snowplow ?driver ?plow ?to)
            (!disengage-plow ?driver ?plow)))

;;quell-riot
  (:method (quell-riot ?loc)
           with-police
           ((in-town ?loc ?town)
            (police-unit ?p1) (police-unit ?p2) (not (equal ?p1 ?p2)))
           ((declare-curfew ?town) (get-to ?p1 ?loc) (get-to ?p2 ?loc)
            (!set-up-barricades ?p1) (!set-up-barricades ?p2)))
  
;;provide-temp-heat
  (:method (provide-temp-heat ?person)
           to-shelter
           ((person ?person) (shelter ?shelter))
           ((get-to ?person ?shelter)))

  (:method (provide-temp-heat ?person)
           local-electricity
           ((person ?person) (atloc ?person ?ploc))
           ((generate-temp-electricity ?ploc) (!turn-on-heat ?ploc)))

;;fix-power-line
  (:method (fix-power-line ?lineloc)
           normal
           ((power-crew ?crew) (power-van ?van))
           ((get-to ?crew ?lineloc) (get-to ?van ?lineloc)
            (repair-line ?crew ?lineloc)))

;;provide-medical-attention
  (:method (provide-medical-attention ?person)
           in-hospital
           ((hospital ?hosp) (has-condition ?person ?cond)
            (not (hospital-doesnt-treat ?hosp ?cond)))
           ((get-to ?person ?hosp) (!treat-in-hospital ?person ?hosp)))

  (:method (provide-medical-attention ?person)
           simple-on-site
           ((has-condition ?person ?cond) (not (serious-condition ?cond)))
           ((emt-treat ?person)))


;;;;;;;;;;;;;;;;;;; subgoals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; clean-up-hazard
  (:method (clean-up-hazard ?from ?to)
           very-hazardous ;; just call the feds
           ((hazard-seriousness ?from ?to very-hazardous))
           ((!call fema))

           normal ;; we can take care of it
           ((hazard-team ?ht))
           ((get-to ?ht ?from) (!clean-hazard ?ht ?from ?to)))

  ;; block-road - blocks off a road
  (:method (block-road ?from ?to)
           normal
           ((police-unit ?police))
           (:unordered (set-up-cones ?from ?to)
            (get-to ?police ?from)))

  ;; unblock-road - unblocks a road
  (:method (unblock-road ?from ?to)
           normal
           ()
           ((take-down-cones ?from ?to)))

  ;; get-electricity provides electricity to a site (if not already there)
  (:method (get-electricity ?loc)
           already-has-electricity ;; do nothing
           ((not (no-electricity ?loc)))
           ()
           
           no-electricity
           ()
           ((generate-temp-electricity ?loc))
           )


  ;; repair-pipe
  (:method (repair-pipe ?from ?to) ;; repairs a pipe at location
           normal
           ((water-crew ?crew))
           ((get-to ?crew ?from)
            (set-up-cones ?from ?to)
            (open-hole ?from ?to)
            (!replace-pipe ?crew ?from ?to)
            (close-hole ?from ?to)
            (take-down-cones ?from ?to)))

  ;; open-hole
  (:method (open-hole ?from ?_to) ;; opens a hole in the street
           normal
           ((backhoe ?backhoe))
           ((get-to ?backhoe ?from)
            (!dig ?backhoe ?from)))

  ;; close-hole
  (:method (close-hole ?from ?_to) ;; opens a hole in the street
           normal
           ((backhoe ?backhoe))
           ((get-to ?backhoe ?from)
            (!fill-in ?backhoe ?from)))

  ;; set-up-cones
  (:method (set-up-cones ?from ?_to) ;; sets up orange cones at road
           normal
           ((work-crew ?crew))
           ((get-to ?crew ?from) (!place-cones ?crew)))

  ;; take-down-cones
  (:method (take-down-cones ?from ?_to) ;; takes down cones
           normal
           ((work-crew ?crew))
           ((get-to ?crew ?from) (!pickup-cones ?crew)))

  ;; clear-wreck
  (:method (clear-wreck ?from ?to) ;; gets rid of a wreck in any loc
           normal
           ((wrecked-vehicle ?from ?to ?veh) (garbage-dump ?dump))
           ((tow-to ?veh ?dump)))

  ;; tow-to - tows a vehicle somewhere
  (:method (tow-to ?veh ?to)
           normal
           ((tow-truck ?ttruck) (vehicle ?veh) (atloc ?veh ?vehloc))
           ((get-to ?ttruck ?vehloc)
            (!hook-to-tow-truck ?ttruck ?veh)
            (get-to ?ttruck ?to)
            (!unhook-from-tow-truck ?ttruck ?veh)))


  ;; clear-tree
  (:method (clear-tree ?tree) ;; this gets rid of a tree in any loc
           normal
           ((tree-crew ?tcrew) (tree ?tree) 
            (atloc ?tree ?treeloc))
           ((get-to ?tcrew ?treeloc) (!cut-tree ?tcrew ?tree)
            (remove-blockage ?tree)))

  ;; remove-blockage
  (:method (remove-blockage ?stuff)
           move-to-side-of-street
           ((work-crew ?crew) (atloc ?stuff ?loc))
           ((get-to ?crew ?loc)
            (!carry-blockage-out-of-way ?crew ?stuff)))

  (:method (remove-blockage ?stuff)
           carry-away
           ((garbage-dump ?dump))
           ((get-to ?stuff ?dump)))

  ;; declare-curfew
  (:method (declare-curfew ?_town)
           normal
           ()
           (:unordered (!call EBS) (!call police-chief)))

  ;; generate-temp-electricity
  (:method (generate-temp-electricity ?loc)
           with-generator
           ((generator ?gen))
           ((make-full-fuel ?gen) (get-to ?gen ?loc) (!hook-up ?gen ?loc)
            (!turn-on ?gen)))

  ;; make-full-fuel - makes sure arg1 is full of fuel
  (:method (make-full-fuel ?gen)
           with-gas-can
           ((gas-can ?gc) (atloc ?gen ?genloc) (service-station ?ss))
           ((get-to ?gc ?ss) (add-fuel ?ss ?gc) (get-to ?gc ?genloc)
            (!pour-into ?gc ?gen)))

  (:method (make-full-fuel ?gen)
           at-service-station
           ((service-station ?ss))
           ((get-to ?gen ?ss) (add-fuel ?ss ?gen)))

  ;; add-fuel (at service-station)
  (:method (add-fuel ?ss ?obj)
           normal
           ()
           (:unordered (!pay ?ss) (!pump-gas-into ?ss ?obj)))

  ;; repair-line
  (:method (repair-line ?crew ?lineloc)
           with-tree
           ((tree ?tree) (atloc ?tree ?lineloc)
            (atloc ?crew ?lineloc))
           ((shut-off-power ?crew ?lineloc) 
            (:unordered (clear-tree ?tree) 
                        (!remove-wire ?crew ?lineloc))
            (!string-wire ?crew ?lineloc) (turn-on-power ?crew ?lineloc))
           
           without-tree
           ((atloc ?crew ?lineloc))
           ((shut-off-power ?crew ?lineloc) 
            (!remove-wire ?crew ?lineloc)
            (!string-wire ?crew ?lineloc) (turn-on-power ?crew ?lineloc)))

  ;; shut-off-power
  (:method (shut-off-power ?_crew ?loc)
           normal
           ((in-town ?loc ?town) (powerco-of ?town ?powerco))
           (!call ?powerco))
           

  ;; turn-on-power
  (:method (turn-on-power ?_crew ?loc)
           normal
           ((in-town ?loc ?town) (powerco-of ?town ?powerco))
           (!call ?powerco))
           
  ;; shut-off-water
  (:method (shut-off-water ?from ?_to)
           normal
           ((in-town ?from ?town) (waterco-of ?town ?waterco))
           ((!call ?waterco)))
           

  ;; turn-on-water
  (:method (turn-on-water ?from ?_to)
           normal
           ((in-town ?from ?town) (waterco-of ?town ?waterco))
           ((!call ?waterco)))
           

  ;; emt-treat
  (:method (emt-treat ?person)
           emt
           ((emt-crew ?emt) (atloc ?person ?personloc))
           ((get-to ?emt ?personloc) (!treat ?emt ?person)))

  ;; stabilize
  (:method (stabilize ?person)
           emt
           ()
           ((emt-treat ?person)))

  ;; get-to
  (:method (get-to ?obj ?place)
           already-there
           ((atloc ?obj ?place))
           ())

  (:method (get-to ?person ?place)
           person-drives-themself
           ((not (atloc ?person ?place))
            (person ?person) (vehicle ?veh) (atloc ?veh ?vehloc)
            (atloc ?person ?vehloc))
           ((drive-to ?person ?veh ?place)))

  (:method (get-to ?veh ?place)
           vehicle-gets-driven
           ((not (atloc ?veh ?place))
            (person ?person)
            (vehicle ?veh) (atloc ?veh ?vehloc)
            (atloc ?person ?vehloc)
            )
           ((drive-to ?person ?veh ?place)))

  (:method (get-to ?obj ?place)
           as-cargo
           ((not (atloc ?obj ?place))
           (vehicle ?veh)
           (atloc ?obj ?objloc)  (fit-in ?obj ?veh)
           (not (non-ambulatory ?obj)))
           ((get-to ?veh ?objloc) (get-in ?obj ?veh) (get-to ?veh ?place)
            (get-out ?obj ?veh))
           
           with-ambulance ;; same as above, just with ambulance
           ((not (atloc ?obj ?place))
            (atloc ?obj ?objloc) (ambulance ?veh) (fit-in ?obj ?veh)
            )
           ((get-to ?veh ?objloc) (stabilize ?obj) (get-in ?obj ?veh)
            (get-to ?veh ?place) (get-out ?obj ?veh))
           )

  (:method (drive-to ?person ?veh ?loc)
           normal
           ((person ?person) (vehicle ?veh) (atloc ?veh ?vehloc)
            (atloc ?person ?vehloc) (can-drive ?person ?veh))
           ((!navegate-vehicle ?person ?veh ?loc)))

  (:method (get-in ?obj ?veh)
           ambulatory-person
           ((atloc ?obj ?objloc) (atloc ?veh ?objloc) 
            (person ?obj) (not (non-ambulatory ?obj)))
           (!climb-in ?obj ?veh)

           load-in
           ((atloc ?obj ?objloc) (atloc ?veh ?objloc)
            (person ?person) (can-lift ?person ?obj))
           ((get-to ?person ?objloc) (!load ?person ?obj ?veh)))

  (:method (get-out ?obj ?veh)
           ambulatory-person
           ((person ?obj) (not (non-ambulatory ?obj)))
           (!climb-out ?obj ?veh)

           unload
           ((atloc ?veh ?vehloc) (person ?person) (can-lift ?person ?obj))
           ((get-to ?person ?vehloc) (!unload ?person ?obj ?veh)))


;;;;;;;;;;;;;;;;;; operators ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (:operator (!navegate-snowplow ?_person ?_veh ?_loc)
             ()
             ()
             ())
  
  (:operator (!engage-plow ?_person ?_plow)
             ()
             ()
             ())

  (:operator (!disengage-plow ?_person ?_plow)
             ()
             ()
             ())

  (:operator (!navegate-vehicle ?person ?veh ?loc)
             ((person ?person) (vehicle ?veh) (atloc ?veh ?vehloc)
              (atloc ?person ?vehloc) (can-drive ?person ?veh)
              (not (wrecked-car ?veh)))
             ((atloc ?veh ?vehloc) (atloc ?person ?vehloc))
             ((atloc ?veh ?loc) (atloc ?person ?loc)))

  (:operator (!climb-in ?obj ?veh) 
             ((atloc ?obj ?objloc) (atloc ?veh ?objloc) (fit-in ?obj ?veh))
             ((atloc ?obj ?objloc))
             ((atloc ?obj ?veh)))

  (:operator (!climb-out ?obj ?veh) 
             ((atloc ?obj ?veh) (atloc ?veh ?vehloc)) 
             ((atloc ?obj ?veh)) 
             ((atloc ?obj ?vehloc)))

  (:operator (!load ?person ?obj ?veh) 
             ((atloc ?obj ?objloc) 
              (atloc ?veh ?objloc) 
              (atloc ?person ?objloc)
              (fit-in ?obj ?veh))
             ((atloc ?obj ?objloc))
             ((atloc ?obj ?veh)))

  (:operator (!unload ?person ?obj ?veh) 
             ((atloc ?obj ?veh) (atloc ?veh ?vehloc) (atloc ?person ?vehloc)) 
             ((atloc ?obj ?veh))
             ((atloc ?obj ?vehloc)))

  (:operator (!treat ?emt ?person) 
             ((atloc ?person ?ploc) (atloc ?emt ?ploc))
             ()
             ())

  (:operator (!treat-in-hospital ?person ?hospital) 
             ((atloc ?person ?hospital))
             ()
             ())

  (:operator (!call ?_place)
             ()
             ()
             ())

  (:operator (!remove-wire ?_crew ?_lineloc)
             ()
             ()
             ())


  (:operator (!string-wire ?_crew ?_lineloc)
             ()
             ()
             ())

  (:operator (!carry-blockage-out-of-way ?_crew ?_stuff)
             ()
             ()
             ())

  (:operator (!cut-tree ?_crew ?_tree)
             ()
             ()
             ())

  (:operator (!hook-up ?_obj ?_loc)
             ()
             ()
             ())

  (:operator (!pourinto ?_obj ?_obj2)
             ()
             ()
             ())

  (:operator (!turn-on ?_obj)
             ()
             ()
             ())

  (:operator (!pay ?_loc)
             ()
             ()
             ())

  (:operator (!pump-gas-into ?_loc ?_obj)
             ()
             ()
             ())

  (:operator (!turn-on-heat ?_loc)
             ()
             ()
             ())
  
  (:operator (!set-up-barricades ?_police)
             ()
             ()
             ())

  (:operator (!place-cones ?_police)
             ()
             ()
             ())

  (:operator (!pickup-cones ?_police)
             ()
             ()
             ())

  (:operator (!hook-to-tow-truck ?_ttruck ?_veh)
             ()
             ()
             ())

  (:operator (!unhook-from-tow-truck ?_ttruck ?_veh)
             ()
             ()
             ())

  (:operator (!dig ?_backhoe ?_place)
             ()
             () 
             ())

  (:operator (!fill-in ?_backhoe ?_place)
             ()
             () 
             ())

  (:operator (!replace-pipe ?_crew ?_from ?_to)
             ()
             () 
             ())

  (:operator (!clean-hazard ?_hazard-team ?_from ?_to)
             ()
             () 
             ())
   
;;;;;;;;;;;;;;;;;;; axioms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; points
  (:- (point ?x)
      ((hospital ?x)))
  (:- (point ?x)
      ((dump ?x)))
  (:- (point ?x)
      ((park ?x)))
  (:- (point ?x)
      ((shelter ?x)))
  (:- (point ?x)
      ((school ?x)))
  (:- (point ?x)
      ((university ?x)))
  (:- (point ?x)
      ((mall ?x)))
  (:- (point ?x)
      ((transport-hub ?x)))
  (:- (point ?x)
      ((service-station ?x)))

  ;; vehicles
  (:- (vehicle ?x)
      ((ambulance ?x)))
  (:- (vehicle ?x)
      ((bus ?x)))
  (:- (vehicle ?x)
      ((power-van ?x)))
  (:- (vehicle ?x)
      ((truck ?x)))
  (:- (vehicle ?x)
      ((police-van ?x)))
  (:- (vehicle ?x)
      ((car ?x)))
  (:- (vehicle ?x)
      ((wrecked-car ?x)))

  (:- (truck ?x)
      ((dump-truck ?x)))
  (:- (truck ?x)
      ((tow-truck ?x)))
  (:- (truck ?x)
      ((water-truck ?x)))
  (:- (truck ?x)
      ((backhoe ?x)))
  (:- (truck ?x)
      ((snowplow ?x)))

  (:- (fit-in ?obj ?veh)
      ((vehicle ?veh) (person ?obj)))
  (:- (fit-in ?obj ?veh)
      ((dump-truck ?veh) (tree ?obj)))
  (:- (fit-in ?obj ?veh)
      ((vehicle ?veh) (generator ?obj)))
  (:- (fit-in ?obj ?veh)
      ((vehicle ?veh) (food ?obj)))

  (:- (can-drive ?person ?veh)
      ((ambulance ?veh) (emt-crew ?person))
      ((power-van ?veh) (power-crew ?person))
      ((truck ?veh) (truck-driver ?person))
      ((bus ?veh) (bus-driver ?person))
      ((police-van ?veh) (police-unit ?person))
      ((tow-truck ?veh) (tow-truck-driver ?person))
      ((backhoe ?veh) (construction-crew ?person))
      ((water-truck ?veh) (water-crew ?person))
      ((snowplow ?veh) (plowdriver ?person))
      default ((not (or (ambulance ?veh) (bus ?veh)
                (truck ?veh) (power-van ?veh) (police-van ?veh)
                (tow-truck ?veh) (backhoe ?veh) (water-truck ?veh)
                (snowplow ?veh)))
       (adult ?person)))

  ;;; people

  (:- (person ?x)
      ((adult ?x)))
  (:- (person ?x)
      ((child ?x)))

  (:- (adult ?x)
      ((emt-crew ?x)))
  (:- (adult ?x)
      ((work-crew ?x)))
  (:- (adult ?x)
      ((bus-driver ?x)))
  (:- (adult ?x)
      ((truck-driver ?x)))
  (:- (adult ?x)
      ((police-unit ?x)))
  (:- (adult ?x)
      ((tow-truck-driver ?x)))
  (:- (adult ?x)
      ((plowdriver ?x)))
  (:- (adult ?x)
      ((shelter-leader ?x)))

  (:- (non-ambulatory ?x)
      ((has-condition ?x broken-back)))

  ;; lifting
  (:- (can-lift ?person ?obj)
      ((not (equal ?person ?obj)) (person ?obj) (emt-crew ?person)))
  (:- (can-lift ?person ?obj)
      ((not (equal ?person ?obj)) (not (person ?obj)) (work-crew ?person)))

  ;; equality
  (:- (equal ?obj1 ?obj2)
      (call eq '?obj1 '?obj2))

  ;; work-crew
  (:- (work-crew ?x)
      (power-crew ?x))
  (:- (work-crew ?x)
      (tree-crew ?x))
  (:- (work-crew ?x)
      (construction-crew ?x))
  (:- (work-crew ?x)
      (water-crew ?x))
  (:- (work-crew ?x)
      (hazard-team ?x))

  ;; roads and locs
  (:- (atloc ?obj ?loc) ;; if an object is on a road, it's at the first town - kludge
      (on-road ?obj ?loc ?_to))

)) ;; end defdomain
