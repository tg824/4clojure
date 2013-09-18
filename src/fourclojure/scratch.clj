(ns fourclojure.scratch)

(defn f1
  []
  (for [x (range 40)
                  :when (= 1 (rem x 4))]
          x))

(f1)
