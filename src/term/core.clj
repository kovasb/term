(ns term.core
  (:use term.protocols)
  (:require [riddley.walk :as rw]
            [riddley.compiler :as rc]
            [term.zip :as z])
  (:import term.protocols.AtomicTerm term.protocols.CompoundTerm)

  (:use term.match))

;; -agentpath:/Applications/YourKit_Java_Profiler_2013_build_13046.app/bin/mac/libyjpagent.jnilib
;;


 (defn to-term [x]
  (if (symbol? x)
    (cond
     (#{'do 'if 'def 'quote 'new} x) x
     ((rc/locals) x) x
     (resolve x) x
     :default (AtomicTerm. x))
    x))

(defmacro term [x] (rw/walk-exprs symbol? to-term x))



(def *evaluate-print* false)

(defn evaluate [init rulefn]
  (loop [node (z/zipper init) pass :down]
                                        ;(println [(z/node node) pass])

    (if *evaluate-print*
      (println [pass (z/node node)]))


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
             (z/node node))))

       ))))


(defn evaluate-stack [init rulefn]
  (if (sequential? init)
    (let [self (z/build init (map #(evaluate-stack % rulefn) init))
          result (rulefn self)]
      (if result
        (evaluate-stack result rulefn)
        self))
    init))



(defn replace-all [init rulefn]
  (loop [node (z/zipper init)]
   ;(println (z/node node))
   (if (z/end? node)
     (z/node node)
     (if-let [m (rulefn  (z/node node))]
       (let [new (z/replace node m)]
         ;; go to next nondescending node
         (if-let [r (z/next-nondescending new)]
           (recur r)
           (z/root new)))
       (recur (z/next node))))))




(defn replace-all-repeated [init rulefn]
  (loop [init init rulefn rulefn]
    (let [result (replace-all init rulefn)]
      (if (not= result init)
        (recur result rulefn)
        result))))




(defn cases [patt expr]
  (loop [node (z/zipper expr) matches []]
    ;(println (z/node node))
    (if (z/end? node)
      matches
      (if (match patt (z/node node))
        (if-let [r (z/next-nondescending node)]
          (recur r (conj matches (z/node node)))
          (conj matches (z/node node)))
        (recur (z/next node) matches)))))




(defn replacement-rule [lhs rhs]
  (fn [input]
    (if-let [m (match lhs input)]
      (eval (replace-all rhs m)))))

(defn rule-list-sub [rules]
  (fn [t] ((rules (head t)
                 (fn [y]
                   nil))
          t)))

(defn meta-rule [rules]
  ;; try rules until one works
  (fn [x]
    (first (keep #(% x) rules))))





(defn pattern-bindings [x]
  (term
   (into #{}
         (map
          first
          (cases ((Verbatim Pattern) (Blank) (Blank)) x)))))

(defn binding-replacements [b]
  (into {} (map
            #(vector % (list 'the-match %))
            b)))

(defn rhs-fn [lhs rhs]
  (let [b  (pattern-bindings lhs)
        replacements (binding-replacements b)]
    (list 'fn
          '[the-match]
          (replace-all rhs replacements))))


(defmacro rule-to-fn [lhs rhs]

  (let [lhs1 (eval (macroexpand (list 'term lhs)))
        rhs1 (macroexpand (list 'term rhs))
        rhsfn (rhs-fn lhs1 rhs1)]
     (println [rhsfn lhs rhs  (binding-replacements (pattern-bindings lhs1))])
     `(let [
          rulefn# #(match ~lhs1 % )
          ]
      (fn [x#]
        (if-let [m# (rulefn# x#)]
          (~rhsfn m#)
          nil)))))


(defmacro rule [lhs rhs]
  (let [lhs1 (eval (macroexpand (list 'term lhs)))
        rhs1 (macroexpand (list 'term rhs))
        rhsfn (rhs-fn lhs1 rhs1)]
    `(reify
       Object
       (toString [this] (str "(Rule " (str ~lhs1) " " (str (quote ~(term rhs1)))  ")"))

       clojure.lang.Sequential
       clojure.lang.Seqable
       (seq [y] (seq [~lhs1 (quote ~rhs1)]))

       ITerm
       (head [this] (AtomicTerm. 'Rule))

       clojure.lang.IFn
       (invoke [this] nil)
       (invoke [this x#]
         (if-let [m# (match ~lhs1 x#)]
           (~rhsfn m#)
           nil)))))

(defn rule-group [rules]
  (let [g (group-by #(head (first %)) rules)]
    (fn [input]
      (let [rule-cases  (g (head input))]
        (if rule-cases
          (reduce
           (fn [dummy rule-case]
             ;(println ["rule" rule-case])
             (if-let [r (rule-case input)]
               (reduced r)
               nil))
           nil
           rule-cases

           ))))))







(comment

  (defn fast-rule
  [x]
  (if (seq? x)
    (let [l1 (first x)]
      (if (seq? l1)
        (if (identical? :e  (first l1))
          (let [l1r (second l1)] (list (list l1r (list :e (second x))) l1r)))))))




 (defn meta-rule [rules]
  ;; try rules until one works
  (fn [x]
    (first (keep #(% x) rules))))




 (def fib-rules22

  [
   (rule (Fib 0) 1)
   (rule (Fib 1) 1)
   (rule (Fib (Pattern a (Blank)))
               (Plus (Fib (- a 1)) (Fib (- a 2))))
   (rule (Plus (Pattern a (Blank Integer))
                     (Pattern b (Blank Integer)))
               (+ a b))
   ])



(def fib-group (rule-group fib-rules22))

(time (evaluate (term (Fib 30)) fib-group))
;; "Elapsed time: 18391.343 msecs"
;; within 9x of mathematica.. still huge slowdown

;; can try w/ global dispatch, but with fast-matching for individual rule cases

(def fib-rules2

  [
   (rule-to-fn (Fib 0) 1)
   (rule-to-fn (Fib 1) 1)
   (rule-to-fn (Fib (Pattern a (Blank)))
               (Plus (Fib (- a 1)) (Fib (- a 2))))
   (rule-to-fn (Plus (Pattern a (Blank Integer))
                     (Pattern b (Blank Integer)))
               (+ a b))
   ])

(def r2 (meta-rule fib-rules2))



(time (evaluate-stack (term (Fib 30)) fib-group))





(def r22 (meta-rule fib-rules2))


  (defn fib [x]
    (if (= x 0) 1
        (if (= x 1) 1
            (+ (fib (- x 1)) (fib (- x 2))))))

  (time (fib 30))
  "Elapsed time: 50.797 msecs"

  ;; 2.237752 seconds in mma; 45x slower
  ;; 6 seconds when written in low level style
  ;; 5 seconds when pattern-match based branching converted to if statement


  )



(comment

  (time (evaluate (term (Fib 10)) r2))
  ;; 4msec

  (time (evaluate (term (Fib 10)) r))
  ;; 155 msec

  (time (evaluate (term (Fib 30)) r2))
  ;; 32 seconds
  ;; 14 slower than mma

  (time (evaluate-stack (term (Fib 30)) r2))
  ;; 26.5 seconds


  ;; tail recursive version
  ;; eliminates call to plus
  (defn fib2 [a b c]
    (if (< c 0)
      a
      (recur b (+ a b) (- c 1))))

  (time (fib2 0 1 30))
  ;; "Elapsed time: 0.093 msecs"

  ;; same in MMA



  (def r3 (rule-to-fn (Fib (Pattern a (Blank))
                    (Pattern b (Blank))
                    (Pattern c (Blank)))
               (if (< c 0)
                 a
                 (Fib b (+ b a) (- c 1)))) )

  (time (evaluate (term (Fib 0 1 30))
                  r3))
  ;; "Elapsed time: 2.566 msecs"
  ;; 30x slower than mma

  (def r4
    (term (fn [x] (if (= Fib (head x) )
                  (let [[a b c] x]
                    (if (< c 0)
                      a
                      (Fib b (+ b a) (- c 1))))))))


  (time (evaluate (term (Fib 0 1 30))
                  r4))
  ;; "Elapsed time: 0.486 msecs"
  ;; 5x slower than mma
  ;; interpreted matching slows us down by 6x on top of this
  ;; trial1: 0.293 msec

    (time (evaluate-stack (term (Fib 0 1 30))
                          r4))
  ;; "Elapsed time: 0.357 msecs"


    (def f (term Fib))

    (let [f (term Fib) p (term Plus)]
      (defn r5 [x]
        (cond
          (= f (head x))
          (let [x1 (first x)]
            (cond
             (= x1 0) 1
             (= x1 1) 1
             :default (p (f (- x1 1)) (f (- x1 2)) )))

          (= p (head x))
             (if (and (number? (first x)) (number? (second x)))
               (+ (first x) (second x))))))


    (time (evaluate (term (Fib 30))
                    r5))
    "Elapsed time: 2699.746 msecs"
    ;; most of time spent in inefficient rule checks?

     (time (evaluate-stack (term (Fib 30))
                           r5))
     "Elapsed time: 2221.857 msecs"
     ;; on par with mathematica!

     ;; try rule dispatch based on head
     ;; note there will be 1 repetative check



    )


(comment

   (def fib-rules
  (term
   [
    (rule2 (Fib 0) 1)
    (rule2 (Fib 1) 1)
    (rule2 (Fib (Pattern a (Blank)))
           (Plus (Fib (- a 1)) (Fib (- a 2))))
    (rule2 (Plus (Pattern a (Blank Integer))
                 (Pattern b (Blank Integer)))
           (+ a b))
    ]))

   (Or
    (== (Cons x) x))

   [A :b (Pattern A (Blank Integer))]


   (rule

     [args ] rhs
     [args2 ] rhs2
     _ Map
     )

   (constructor
     Map
     [args] rhs
     [args] rhs

     )


   (defrecord AtomicTerm [symbol var]
              Ifn


              )


(defmacro rule2 [lhs rhs]
  (let [z1 (macroexpand (list 'term rhs) )]
    `(reify
      Object
      (toString [this] (str "(Rule " (str ~lhs) " " (str (quote ~(term rhs)))  ")"))

      clojure.lang.Sequential
      clojure.lang.Seqable
      (seq [y] (seq [~lhs (quote ~rhs)]))

      ITerm
      (head [this] (AtomicTerm. 'Rule))

      clojure.lang.IFn
      (invoke [this] nil)
      (invoke [this z#]
        (if-let [m# (match  ~lhs z#)]
          (eval (replace-all (quote ~z1) m#)))))))

(defmacro rule3 [lhs rhs]
  (let [z1 (macroexpand (list 'term rhs) )]
    `(reify
      Object
      (toString [this] (str "(Rule " (str ~lhs) " " (str (quote ~(term rhs)))  ")"))

      clojure.lang.Sequential
      clojure.lang.Seqable
      (seq [y] (seq [~lhs (quote ~rhs)]))

      ITerm
      (head [this] (AtomicTerm. 'Rule))

      clojure.lang.IFn
      (invoke [this] nil)
      (invoke [this z#]
        (if-let [m# (match  ~lhs z#)]
          (replace-all  ~z1 m#))))))

(defmacro rule [lhs rhs]
  `(fn [input#]
     (if-let [m# (match (term ~lhs) input#)]
       (eval (replace-all (term (quote ~rhs)) m#)))))


  (defmacro rule [lhs rhs]
   (let [lhs1 (eval (macroexpand (list 'term lhs)))
         rhs1 (macroexpand (list 'term rhs))
         rhsfn (rhs-fn lhs1 rhs1)

         ]
     `(reify
        Object
        (toString [this] (str "(Rule " (str ~lhs1) " " (str (quote ~(term rhs1)))  ")"))

        clojure.lang.Sequential
        clojure.lang.Seqable
        (seq [y] (seq [~lhs1 (quote ~rhs1)]))

        ITerm
        (head [this] (AtomicTerm. 'Rule))

        clojure.lang.IFn
        (invoke [this] nil)
        (invoke [this x#]
          (if-let [m# (match ~lhs1 x#)]
            (~rhsfn m#)
            nil))))))


;; minimal checking
;; find pattern paths
;; for each collection containing a pattern path, need to manually check the elements at that level
;; subtrees containing no patterns can be compared directly

;; how to bubble wrappers?




;; better to built a JITy matcher - use same walk as for normal matching, but record the sequence of traversals, bindings and tests
;; gensyms correspond to unique ids on the tree
;; nth etc correspond to node value extraction / movement operations


(comment
  (def transform-rules
   (term
    [(rule2
      (Transform (Pattern sym (Blank Symbol))
                 (Pattern value (Blank Integer)))
      (Equal? sym value))

     (rule2
      (Transform (Pattern sym (Blank Symbol))
                 (Pattern value (Blank Vector)))
      (If (Vector? sym)
          (reduce
           (fn [z y] (If y z))
           (reverse
            (map-indexed
             (fn [i x]
               (let [s (AtomicTerm. (gensym))]
                 (Let [s (Nth sym i)]
                      (Transform s (nth value i)))))
             value)))))

     ])))





(comment
  (defn transform [symbol form]
   (cond
    (number? form) `(== ~symbol ~form)

    (vector? form)
    `(if (vector? ~symbol)
       ~(reduce
         (fn [x y]
           `(if  ~y  ~x))
         (reverse
          (map-indexed
           (fn [i x]
             (let [s (gensym)]
               `(let [~s (nth ~symbol ~i)]
                  ~(transform s (nth form i)))))
           form)))))))



(comment

  (defn sample-combinator
  [x]
  (if (seq? x)
    (let [l1 (first x)]
      (if (seq? l1)
        (if (identical? :e  (first l1))
          (let [l1r (second l1)] (list (list l1r (list :e (second x))) l1r)))))))

  (time
   (loop [i 0 term (list (list (list :e (list (list :e :e) :e)) :e)  :e)]
     (when-not (> i 300)
       (recur (+ 1 i) (replace-all term sample-combinator)))))
  ;; "Elapsed time: 231.631 msecs"


  (defn sample-combinator2
  [x]
  (term (if (seq? x)
     (let [l1 (first x)]
       (if (seq? l1)
         (if (=  e  (first l1))
           (let [l1r (second l1)] (list (list l1r (list e (second x))) l1r))))))))


   (time
   (loop [i 0 t (term (list (list (list e (list (list e e) e)) e)  e))]
     (when-not (> i 300)
       (recur (+ 1 i) (replace-all t sample-combinator2)))))
   ;; "Elapsed time: 239.816 msecs"
   ;; incrementally slower than combinator

   (defn sample-combinator3
  [x]
  (term (if (sequential? x)
     (let [l1 (first x)]
       (if (sequential? l1)
         (if (=  e  (first l1))
           (let [l1r (second l1)] (A (A l1r (A e (second x))) l1r))))))))

   (time
   (loop [i 0 t (term (A (A (A e (A (A e e) e)) e)  e))]
     (when-not (> i 300)
       (recur (+ 1 i) (replace-all t sample-combinator3)))))
   ;; "Elapsed time: 332.606 msecs"
   ;; more intensive constructor process



  )


(comment
  (def c1 (chan))
  (def c2 (chan))
  (def c3 (chan))
  (def c4 (chan))

  (go
   (let [f {"a" (fn [x] (>! c1 x) )
            "b" (fn [x] (loop [y 0]
                         (if (> y 5)
                           1
                           (do (>! c2 x) (recur (+ 1 y))))))}]
     (while true
       ((f (<! c3)) (<! c4)))
     )

   )

  ;; indicate bindings via metadata?
  ^{:bind [a b c]} (C a)
  (bind [a b] (C a))
  (Bind [a b] (C a))
  (fresh [a b] [a b])
  (Foo a b c :bind [a])

  ;; or, force custom binding constructs to impl protocols
  (SomeTerm. x bindings)
  ;; but then need a constructor function
  (Fn (Fresh [a b]) )
  ((Fresh [a b] Fn) x)
  ((Fn (Fresh a b) (Plus a b)))


  Attributes [fn]
  )


(comment


  (def Map
    (term-rules
      Map
      [a b c] rhs1
      [?a ?b ?c] rhs2


     )

    )


  (def Map
    (fn
      ([a b]
       (if (good? a b)
         (map a b)
         (construct-term Map a b)))
      ())
    )
















  )