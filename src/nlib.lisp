(in-package :monroe)
;; my library functions for lisp
;; just put link to this where code is
;; someday will do something smarter

;; (defun nlib ()
;;   ;; loads this file
;;   (load "nlib"))

(defun perljoin (separator list)
        ;; like perl's join command, takes a list of strings
        ;; and returns a single string with the strings separated
        ;; with separator
        (if (endp list)
                        ;; list was empty to begin with
                        ""
                (if (endp (cdr list))
                                ;; we are on the last item
                                (format nil "~A" (car list))
                        ;; there are still items left
                        (format nil "~A~A~A"
                                                        (car list)
                                                        separator
                                                        (perljoin separator (cdr list))))))

(defun remove-first (elem list)
  ;; does a remove of only the first occurance of elem
  (remove elem list :count 1))

(defun every-other (lst skip start)
  ;; returns a list from taking every skip items from lst starting at
  ;; element start
  (if (< (length lst) (+ 1 start)) ;; list too small to get anything
      '()
    (cons (elt lst start)
          (every-other (nthcdr (+ start skip 1) lst )
                       skip
                       0))))

(defun remove-pos (lst pos)
  ;; returns a copy of lst with element pos removed
  ;; I have no idea why this doesn't already exist in lisp - probably does
  ;; check values
  (if (or (< pos 0) (>= pos (length lst))) ;; bad pos
      (error "bad position (~A) for list ~A." pos lst)
    ;; special case if pos is last
    (if (= pos (- (length lst) 1))
        (subseq lst 0 pos)
      (append (subseq lst 0 pos)
              (subseq lst (+ pos 1))))))

(defun freqlist-to-problist (list)
  ;; converts a list of items and numbers corresponding to 
  ;; liklihood (frequency)
  ;; to a list where the numbers are probabilities
  (let ((total (reduce (lambda (item1 item2) ;; get the total
                         (+ item1 (cdr item2)))
                       list
                       :initial-value 0)))
    ;; divide each num by total
    (mapcar (lambda (item)
              (cons (car item)
                    (* 1.0 (/ (cdr item) total))))
            list)))

(defun problist-to-list (plist)
        ;; given a problist, returns a list of just the real values
        ;; without their probabilities
        (mapcar (lambda (item)
                                                (car item))
                                        plist))

(defun problist-choose-stochastic-pos (plist)
  ;; chooses a position in the alist based on the probabilities
  ;; given in the problist (an alist of item . prob)
  (labels
   ((choose-aux (plist num pos) ;; aux for recursion, decrement num w/ prob
        (let ((newnum (- num (cdar plist)))) ;; decrement
          (if (< newnum 0)
              ;; then choose this item
              pos
            ;; else keep looking
            (choose-aux (cdr plist) newnum (1+ pos))))))
  (let ((ran (random 1.0))) ;; generate random num
    (choose-aux plist ran 0)))
  )

(defun problist-choose-stochastic (plist)
  ;; stocastically chooses and returns an element of plist (w/o the prob)
  (let ((pos (problist-choose-stochastic-pos plist)))
    (car (nth pos plist))))

(defun list-choose-uniform-pos (lst)
  ;; uniformly chooses a list pos
  (random (length lst)))

(defun list-choose-uniform (lst)
  ;; randomly (uniformly) chooses one of the items in the list and
  ;; returns it
  (nth (list-choose-uniform-pos lst) lst))

(defun list-choose-uniform-and-remove (listform)
  ;; randomly chooses a list item, removes it and returns it
  ;; this is not destructive
  (let ((pos (list-choose-uniform-pos listform)))
     (remove-pos listform pos)))

(defun mymaphash (fn hash)
  ;; takes a function with 2 args (key val) and does a maphash
  ;; kind of thing on it, but this returns a list of
  ;; results, like maplist does
  (with-hash-table-iterator (iter hash)
    (do ((item (multiple-value-list (iter)) (multiple-value-list (iter)))
          (res '() (cons (funcall fn (second item) (third item)) res)))
         ((null (car item)) res))))

(defun gethashkeys (hash)
  ;; returns a list of all the keys for the hash
  (mymaphash (lambda (key val) (declare (ignore val)) key) hash))

(defun gethashvals (hash)
  ;; returns a list of all the values in the hash
  (mymaphash (lambda (key val) (declare (ignore key)) val) hash))

(defun randomize-list (lst)
  "Returns a copy of LST with all its members in a random order."
  (nshuffle-list lst))

;;; Taken from Rosetta Code, this is an implementation of the
;;; Knuth/Fisher-Yates shuffle algorithm for generating a random
;;; permutation.  The original RANDOMIZE-LIST did not correctly do
;;; this. [2017/05/15:rpg]
(defun nshuffle (sequence)
  (etypecase sequence
    (list  (nshuffle-list sequence))
    (array (nshuffle-array sequence))))
 
(defun nshuffle-list (list)
  "Shuffle the list using an intermediate vector."
  (let ((array (nshuffle-array (coerce list 'vector))))
    (declare (dynamic-extent array))
    (map-into list 'identity array)))
 
(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
        do (rotatef (aref array (random i))
                    (aref array (1- i)))
        finally (return array)))