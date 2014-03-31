(ns bench.core
  (:require-macros [bench.macros :as m])
  (:require [bench.evaluate :as e]
            [bench.zip :as z])
  )

(defn js-print [& args]
  (if (js* "typeof console != 'undefined'")
    (.log js/console (apply str args))
    (js/print (apply str args))))

(set! *print-fn* js-print)

(comment
  (defn result []
   (let []
     (iterate  #(e/replace-all % e/rule-lowlevel2)
               '(((:e ((:e :e) :e)) :e) :e)))))

(time
 (def result (let []
               (iterate  #(e/replace-all % e/rule-lowlevel2)
                         '(((:e ((:e :e) :e)) :e) :e)))))


(time "foo")

(time (nth result  200))

(println "debug")


(comment

  (defrecord Test [a])

(m/timing (dotimes [x 1000000] (Test. x)))

(let [t (Test. 1)] (m/timing (dotimes [x 1000000] (assoc t :a 1 ))))

(deftype Test2 [a])

;(println ( (Test. 1) :a))

(m/timing (dotimes [x 1000000] (Test2. x)))

(time (dotimes [x 1000000] (Test2. x)) )

(def t2 (Test2. 1))

(m/timing (dotimes [x 1000000]  (.-a ^Test t2)))

(let [t {}] (time (dotimes [x 1000000] (assoc t :a x))))

(println "hashmap")

(let [t {}] (m/timing (dotimes [x 1000000] (assoc t :a x))))

(println "vector")

(m/timing (dotimes [x 1000000] [x]))

(m/timing (dotimes [x 1000000] (apply vector [x])))

(m/timing (dotimes [x 1000000] (into [] [x])))

(m/timing (dotimes [x 1000000] (vector x)))

(m/timing (dotimes [x 1000000] (first [1])))


(println "list")

(m/timing (dotimes [x 1000000] (list x)))

(m/timing (dotimes [x 1000000] (apply list [x])))

(m/timing (dotimes [x 1000000] (into '() [x])))

(m/timing (dotimes [x 1000000] (first '(1))))

;(println (. (js/Date. ) (getMillseconds)))

(println "result")

(time
 (def result (let []
               (iterate  #(e/replace-all % e/rule-lowlevel2)
                         '(((:e ((:e :e) :e)) :e) :e)))))

;(println (. (js/Date. ) (getMillseconds)))


(println "bigresult")

;(println (. (js/Date. ) (getMillseconds)))


(time )

;(time  (println (count (nth result 300))) )


  )




;(println (. (js/Date. ) (getMillseconds)))


;(println bigresult )

;(println (. (js/Date. ) (getMillseconds)))
