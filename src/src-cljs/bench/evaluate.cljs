(ns bench.evaluate

  (:require [bench.zip :as z])

  )

;; -agentpath:/Applications/YourKit_Java_Profiler_2013_build_13046.app/bin/mac/libyjpagent.jnilib
;;


(defn evaluate [init rulefn]
  (loop [node (z/vector-zip init) pass :down]
    ;(println [(z/node node) pass])
    (cond

     (= pass :down)
     (if-let [d (z/down node)]
       ;; if we have children, evaluate them first
       (recur d :down )
       ;; if no children, evaluate self
       (recur node :self))

     (= pass :self)
     (let [newval (rulefn (z/node node))]
       ;; if rule fires
       (if newval
         ;; replace node with new value
         (let [newnode (z/replace node newval)]

           ;; following branch is an optimization
           (if (number? newval)
             ;; if new value is primitive, we can skip its evaluation cycle and move to the next node
             ;; if there is a right sibling, move to the right and begin evaluation
             (if-let [r (z/right newnode)]
               (recur r :down)
               ;; if no right sibling, this level is done and we move to evaluate parent
               (if-let [u (z/up newnode)]
                 (recur (z/up newnode) :self)
                 ;; if no parent, we are done with evaluation and return the toplevel value
                 (z/node newnode)))

             ;; if replacement is not a primitive, then evaluate it
             (recur newnode  :down)))
         ;; match not fired, move to next node
         (if-let [r (z/right node)]
           ;; if has a right sibling, move there
           (recur r :down)
           ;; if no right sibling, move to parent if it exists
           (if-let [u (z/up node)]
             (recur (z/up node) :self)
             ;; otherwise we are done with evaluation, return toplevel form
             (z/node node))))))))

(defn replace-all [init rulefn]
  (loop [node (z/vector-zip init)]
   (if (z/end? node)
     (z/node node)
     (if-let [m (rulefn  (z/node node))]
       (let [new (z/replace node m)]
         (if-let [r (z/right new)]
           (recur r)
           (z/root new)))
       (recur (z/next node))))))



(def rule-lowlevel2
  (fn [x]
    (if (seq? x)
      (let [l1 (first x)]
        (if (seq? l1)
          (if (identical? :e (first l1))
            (let [xr (second l1) yr (second x)]
              (list (list xr (list :e yr) ) xr )   ;[[xr [:e yr]] xr]
              )
            false)
          false))
      false)))

(comment
  (do (def result (let []
                    (iterate  #(replace-all % rule-lowlevel2)
                              '(((:e ((:e :e) :e)) :e) :e))))
      1))


; [ [ [:e [ [:e :e] :e] ] :e]   :e]

(comment
  (time (do (def bigresult (nth result 300)) 1)))
