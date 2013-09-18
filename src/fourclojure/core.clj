(ns fourclojure.core)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!")
  (println x "fourclojure.core is the best"))

(defn -main
  []
  (foo "x"))

(defn some-func
  []
  "testing :Gcommit")
