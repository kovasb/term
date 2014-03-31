(ns term.protocols)

(defprotocol ITerm
  (head [x]))

(defprotocol IWrappedTerm
  (inner-value [x]))

;; also need to implement equality semantics
;; cannot reuse Records b/c they are already sequable

(deftype CompoundTerm [h b]

  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [x] (seq b))
  ITerm
  (head [x] h)
  clojure.lang.IFn
  (invoke [this] (CompoundTerm. this []))
  (invoke [this x1] (CompoundTerm. this [x1]))
  (invoke [this x1 x2] (CompoundTerm. this [x1 x2]))
  (invoke [this x1 x2 x3] (CompoundTerm. this [x1 x2 x3]))
  (invoke [this x1 x2 x3 x4] (CompoundTerm. this [x1 x2 x3 x4]))
  (invoke [this x1 x2 x3 x4 x5] (CompoundTerm. this [x1 x2 x3 x4 x5]))
  (applyTo [this arglist] (CompoundTerm. this arglist))

  Object
  (equals [this x] (and (.equals h (head x)) (= (seq b) (seq x))) )
  (hashCode [this] (+ (* 31 (hash h)) (hash (seq b))) )
  (toString [this] (str "(" (head this) " " (str (seq b))  ")"))
  )


(deftype AtomicTerm [h]
  IWrappedTerm
  (inner-value [x] h)
  ITerm
  (head [x] (head h))
  clojure.lang.Seqable
  (seq [x] nil)
  clojure.lang.IFn
  (invoke [this] (CompoundTerm. this []))
  (invoke [this x1] (CompoundTerm. this [x1]))
  (invoke [this x1 x2] (CompoundTerm. this [x1 x2]))
  (invoke [this x1 x2 x3] (CompoundTerm. this [x1 x2 x3]))
  (invoke [this x1 x2 x3 x4] (CompoundTerm. this [x1 x2 x3 x4]))
  (invoke [this x1 x2 x3 x4 x5] (CompoundTerm. this [x1 x2 x3 x4 x5]))
  (applyTo [this arglist] (CompoundTerm. this arglist))

  Object
  (equals [this x]
    (and (instance? AtomicTerm x)
         (.equals h (inner-value x))))
  (hashCode [this] (hash h))
  (toString [this] (str h))
  )







(extend-protocol
    ITerm
  java.lang.Long
  (head [x] Integer)
  clojure.lang.Symbol
  (head [x] (AtomicTerm. 'Symbol) )
  java.lang.String
  (head [x] String)
  java.lang.Double
  (head [x] (AtomicTerm. 'Real) )
  clojure.lang.Ratio
  (head [x] (AtomicTerm. 'Rational) )
  clojure.lang.Keyword
  (head [x] (AtomicTerm. 'Keyword)))


(extend-type
    Object
  ITerm
  (head [x]
    (cond
     (vector? x)
     (do
       (extend-type
           (class x)
         ITerm
         (head [x] (AtomicTerm. 'Vector)))
       (head x))

     (map? x)
     (do
       (extend-type
           (class x)
         ITerm
         (head [x] (AtomicTerm. 'Map)))
       (head x))


     (set? x)
      (do
       (extend-type
           (class x)
         ITerm
         (head [x] (AtomicTerm. 'Set)))
       (head x))


     (sequential? x)
     (do
       (extend-type
           (class x)
         ITerm
         (head [x] (AtomicTerm. 'Seq)))
       (head x))

     :default (AtomicTerm. 'Object)   ;(throw (Exception. "No ITerm implementation"))
     )))
