(ns fourclojure.problem83)


(defn problem83 
  "Write a function which takes a variable number of booleans. 
   Your function should return true if some of the parameters are true, but not all of the parameters are true. 
   Otherwise your function should return false." 
  [& args]
  (let [argc (count args)
        argt (count (filter #(= true %) args)) ]
    (if (and (not= argc argt) (> argt 0))
      true
      false)))

(problem83 false true) 
(= false (problem83 false false))

