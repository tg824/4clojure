(ns fourclojure.core
  (require [clojure.string :as string]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn -main
  []
  (foo "x"))

(defn some-func
  []
  "testing :Gcommit")

(def themap {:a nil :b 2})
(nil? (:a themap))


(nilkey :a {:a nil :b 2 :c 3})
(defn nilkey
  [k m]
  (and (contains? m k) (nil? (k m))))


(nilkey :c {:a nil :b 2})
(contains? {:a nil :b 2} :c)
(:c {:a nil :b 2})

(and (contains? themap :c) (nil? (:c themap)))

; 30 
; Write a function which removes consecutive duplicates from a sequence.
(defn prob30
  [arglist]
  (when-let [[f & r] (seq arglist)]
    (if (= f (first r))
      (prob30 r)
      (cons f (prob30 r)))))
   
(= (apply str (prob30 "Leeeeeerrroyyy")) "Leroy")
(= (prob30 [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
(= (prob30 [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

; my-flatten
(defn mf
  [coll]
  (when-let [s (seq coll)]
    (if (coll? (first s))
      (concat (mf (first s)) (mf (rest s)))
       (cons (first s) (mf (rest s))))))
(mf '(1 (2 3) 4))
; turns into
(cons 1 (concat (cons 2 (cons 3 (mf nil))) (cons 4 (mf nil))))
(concat (cons 2 (cons 3 (mf nil))) (cons 4 (mf nil)))
(= (mf '((((:a))))) '(:a))
(mf '((((:a)))))
(mf '((1 2) 3 [4 [5 6]]))
(mf '(1 (2) 3))
(defn flatten2
 [coll]
 (when-let [s (seq coll)]
   (if (coll? (first s))
     (concat (flatten2 (first s)) (flatten2 (rest s)))
       (cons (first s) (flatten (rest s))))))


(defn compress
[coll]
  (when-let [[f & r] (seq coll)]
   (if (= f (first r))
     (compress r)
       (cons f (compress r)))))

(compress '(1 2 2 3 3 2))
; turns into recursively
(cons 1 (cons 2 (cons 3 (cons 2 (compress nil)))))

(compress [1 1 1 2 2 2 3 3 3 2 2 2 4 33 3 3 2 ])


 ; compress without using first and rest in when-let
  (defn compress-nofr
    [coll]
    (when-let [s (seq coll)]
      (if (= (first s) (first (rest s)))
        (compress-nofr (rest s))
          (cons (first s) (compress-nofr (rest s))))))
 
(compress '(1 1 1 2 2 2 3 3 3))
(flatten2 '(1 2 (3 4) 5 ((((6))))))


; Problem 31 - Write a function which packs consecutive duplicates into sub-lists
(defn pack
 [coll]
  (partition-by identity coll))
; partition-by applies f to each value in coll, splitting it each time f returns
; a new value.  Returns a lazy sequence of partitions.  So it applies identity to
; each element, and splits when it encounters a different element.
(= (pack [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
(= (pack [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
(= (pack [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))

; Problem 32 - Write a function which duplicates each element of a sequence.
(defn dupe
  [coll]
  (when-let [s (seq coll)]
    (cons (first s) (cons (first s) (dupe (rest coll))))))

(defn dupe2
  [coll]
    (when-let [s (seq coll)]
      (conj (dupe2 (rest coll)) (first s) (first s))))

(defn dupe-with-reduce
  [coll]
    (reverse
       (reduce (fn [a b]
         (cons b (cons b a))) '() coll)))
(dupe-with-reduce [1 2 3])
(= (dupe [1 2 3]) '(1 1 2 2 3 3))
(dupe2 '(1 2 3))
(= (dupe [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
(= (dupe [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(= (dupe [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))


(apply concat [[1 2] [3 4]])
(concat [[1 2] [3 4]])

; Problem 33 - Write a function which replicates each element of a
; sequence a variable number of times.
(defn replic
 [coll n]
   (mapcat #(repeat n %) coll))
  
(replic [[1 2] [3 4]] 2)
(replic [1 2 3] 2)
(= (replic [1 2 3] 2) '(1 1 2 2 3 3))
(= (replic [:a :b] 4) '(:a :a :a :a :b :b :b :b))
(= (replic [4 5 6] 1) '(4 5 6))
(= (replic [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
(= (replic [44 33] 2) [44 44 33 33])
(replic [44 33] 2)


; Problem 34 - Write a function which creates a list of all integers in a given range.
(defn myrange
  [lo hi]
  (when-not (= lo hi)
    (cons lo (lazy-seq (myrange (inc lo) hi)))))
(= (myrange 1 4) '(1 2 3))
(= (myrange -2 2) '(-2 -1 0 1))
(= (myrange 5 8) '(5 6 7))


; Problem 39 - Write a function which takes two sequences and returns the first item from each,
; then the second item from each, then the third, etc.
(defn my-interleave
  [c1 c2]
    (let [s1 (seq c1) s2 (seq c2)]
      (if (and s1 s2)
        (cons (first s1) (cons (first s2)
         (my-interleave (rest s1) (rest s2)))))))

(= (my-interleave [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
(= (my-interleave [1 2] [3 4 5 6]) '(1 3 2 4))
(= (my-interleave [1 2 3 4] [5]) [1 5])
(= (my-interleave [30 20] [25 15]) [30 25 20 15])

; Problem 40 - Interpose a sequence
(defn my-interpose
  [theval coll]
    (when-let [s (seq coll)]
      (if-not (empty? (rest s))
        (cons (first s) (cons theval (my-interpose theval (rest coll))))
          (cons (first s) (my-interpose theval (rest coll))))))
(= (my-interpose 0 [1 2 3]) [1 0 2 0 3])
(= (apply str (my-interpose ", " ["one" "two" "three"])) "one, two, three")
(= (my-interpose :z [:a :b :c :d]) [:a :z :b :z :c :z :d])


; Problem 41 - Write a function which drops every Nth item from a sequence.
(defn drop-nth
 [coll n]
  (mapcat (partial take (dec n)) (partition-all n coll)))
(= (drop-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (drop-nth [:a :b :c :d :e :f] 2) [:a :c :e])
(= (drop-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])

; Problem 42- Write a function which calculates factorials
(defn fac
  [n]
 (reduce * (filter pos? (range (inc n)))))
(= (fac 1) 1)
(= (fac 3) 6)
(= (fac 5) 120)
(= (fac 8) 40320)
