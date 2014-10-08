(ns term.match
  (:use term.protocols)
  (:require [term.zip :as z])
  (:import term.protocols.AtomicTerm term.protocols.CompoundTerm))


;; match requires carrying around state during the traversal:
;; 1. match bindings
;; 2. repeated patterns need validation that they are the same

;; if match fails, return from loop immediately (potentially with the position, or with a partial tree)
;;



(defn pattern? [x]
  (= (AtomicTerm. 'Pattern) (head x)))

(defn pattern-name [x]
  (first x))

(defn parent-is-pattern? [p]
  (if (z/up p) (pattern? (z/node (z/up p)))))


(defn blank? [p]
  (= (AtomicTerm. 'Blank) (head p)))

(defn blank-match? [p d]
  (if-let [f (first p)]
    (= f (head d))
    true))

(def *match-print* false)

(defn verbatim? [x]
  (= (AtomicTerm. 'Verbatim) (head x)))


(defn match [pattern data]
  (loop [p (z/zipper pattern) d (z/zipper data) matches {} pass :match]
    (if *match-print*  (println [p d matches pass ]))
    (cond

     (and (= nil p) (= nil d))
     matches

     (and (z/end? p) (z/end? d))
     ;; if at the end, return matches
     matches


     (and (= :next-nondescending pass) (parent-is-pattern? p))
     ;; bind pattern variable, move up in pattern tree, recur with :next-nondescending
     (recur (z/up p) d
            (assoc matches (pattern-name (z/node (z/up p))) (z/node d))
            :next-nondescending)

     (and (= :next-nondescending pass) (not (parent-is-pattern? p)))
     ;; move right with pass :match, or go up with :next-nondescending
     (if (z/right p)
       (recur (z/right p) (z/right d) matches :match)
       (recur (z/up p) (z/up d) matches :next-nondescending))


     (and (= :match pass) (pattern? (z/node p)))
     ;; move into pattern body with pass :match
     (recur (z/right (z/down p)) d matches :match)


     (and (= :match pass) (not (pattern? p)))
     ;; match logic
     (let [pnode (z/node p) dnode (z/node d)]
       (cond

        ;; if literal match, recur with :next-nondescending
        (= pnode dnode)
        (recur p d matches :next-nondescending)

        ;; if blank, check blank pattern
        (blank? pnode)
        (if (blank-match? pnode dnode)
          (recur p d matches :next-nondescending)
          nil)

       ;; if a compound sequential structure, go down with :match
        (and (or (= (head pnode) (head dnode))
                 ;; hack for (Verbatim Pattern)
                 (and (verbatim? (head pnode))
                      (= (first (head pnode)) (head dnode))))
            (sequential? dnode))
       (recur (z/down p) (z/down d) matches :match)

       :default nil

       )))))









(comment
  ;; need something for non-sequentials - maps, sets

  ;; what does matching on sets look like?
  #{[:pattern :a] [:pattern :b]}
  ;; for each element in pattern set, need to find equiv elt in data
  ;; disallow patterns?

  ;; for hashmaps, also disallow patterns in keys?

  (z/general-right next-element next-right)
  (z/general-right :foo)

  {:keys [a b c]}


  {:a [:pattern x]}


  )
