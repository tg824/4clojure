(ns fourclojure.core
  (require [clojure.string :as string]
           [clojure.set :as set])
  (import (java.lang Integer)))

(defn foo
  "I don't do a whole lot." [x]
  (println x "Hello, World!"))

(defn -main
  []
  (foo "x"))

(defn some-func
  []
  "testing :Gcommit")

(def themap {:a nil :b 2})
(nil? (:a themap))


(defn nilkey
  [k m]
  (and (contains? m k) (nil? (k m))))
(nilkey :a {:a nil :b 2 :c 3})


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

; Problem 43 - Write a function which reverses the interleave process into x number of subsequences.
(defn rev-interleave
  [coll n])
(= (rev-interleave [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
(= (rev-interleave (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
(= (rev-interleave (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
(map list (partition 2 [1 2 3 4 5 6]))
(apply map list (partition 2 [1 2 3 4 5 6]))
(partition 2 [1 2 3 4 5 6])
(map list (partition 2 [1 2 3 4 5 6]))

; Problem 83
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

(do (loop [x 5
       result []]
  (if (> x 0)
    (recur (dec x) (conj result (+ 2 x)))
    result)))

; Problem 69 - Merge with a Function
; Write a function which takes a function f and a variable number of maps. 
; Your function should return a map that consists of the rest of the maps 
; conj-ed onto the first. If a key occurs in more than one map, the mapping(s) 
; from the latter (left-to-right) should be combined with the mapping in the 
; result by calling (f val-in-result val-in-latter)
(defn merge-with-a-function
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)] 
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
          (reduce merge2 maps))))

(=  (merge-with-a-function *  {:a 2, :b 3, :c 4}  {:a 2}  {:b 2}  {:c 5})
      {:a 4, :b 6, :c 20})
(=  (merge-with-a-function -  {1 10, 2 20}  {1 3, 2 10, 3 15})
      {1 7, 2 10, 3 15})
(=  (merge-with-a-function concat  {:a  [3], :b  [6]}  {:a  [4 5], :c  [8 9]}  {:b  [7]})
      {:a  [3 4 5], :b  [6 7], :c  [8 9]})

; Problem 58 - Function composition
; Write a function which allows you to create function compositions. 
; The parameter list should take a variable number of functions, and create a 
; function applies them from right-to-left.


(defn my-comp [& fns] 
  (let [fs (reverse fns)]
    (fn [& args]
      (loop [acc (apply (first fs) args)
             fs (next fs)]
        (if fs
          (recur ((first fs) acc) (next fs))
          acc)))))

(=  [3 2 1]  ((my-comp rest reverse)  [1 2 3 4]))
(= 5  ((my-comp  (partial + 3) second)  [1 2 3 4]))
(= true  ((my-comp zero? #(mod % 8) +) 3 5 7 9))
(= true  ((my-comp zero? #(mod % 8) +) 3 5 7 9))

; Problem 59 - Juxtaposition
; Take a set of functions and return a new function that takes a variable number 
; of arguments and returns a sequence containing the result of applying each 
; function left-to-right to the argument list.
(defn my-juxt
  [& fns]
  (fn [& args]
    (loop [acc [] 
           fs (seq fns)]
      (if fs 
        (recur (conj acc (apply (first fs) args)) (next fs))
        acc))))
((my-juxt + max min) 2 3 5 1 6 4)
(=  [21 6 1]  ((my-juxt + max min) 2 3 5 1 6 4))
(=  ["HELLO" 5]  ((my-juxt #(.toUpperCase %) count) "hello"))
(=  [2 6 4]  ((my-juxt :a  :c :b)  {:a 2, :b 4, :c 6, :d 8 :e 10}))

; or
(((fn [& fns]
  (fn [& args]
    (for [f fns] (apply f args)))) + max min) 2 3 5 1 6 4)

; Problem 60 - Sequence Reductions
; Write a function which behaves like reduce, but returns each intermediate 
; value of the reduction. Your function must accept either two or three arguments, 
; and the return sequence must be lazy.
; cons finishes off with nil, that's why when-let works
(defn my-reductions
  ([f coll]
   (lazy-seq
     (if-let [s (seq coll)] 
       (my-reductions f (first coll) (rest coll)) ; call reductions with first element in collection as the init value
       (list (f))))) ; otherwise return the function
  ([f init coll]
   (cons init   ; function call at the top works because the 3-arity version is a cons.  You're consing the result of f applied to the previous value and head of list
         (lazy-seq
           (when-let [s (seq coll)] 
             (my-reductions f (f init (first s)) (rest s))))))) ; recursive call: f, result of f applied to the first element in seq

(=  (take 5  (my-reductions +  (range)))  [0 1 3 6 10])
(=  (my-reductions conj  [1]  [2 3 4])  [[1]  [1 2]  [1 2 3]  [1 2 3 4]])
(=  (last  (my-reductions * 2  [3 4 5]))  (reduce * 2  [3 4 5]) 120)

; Problem  61 - Implement zipmap
(defn my-zipmap
  [keys vals]
  (apply hash-map (mapcat list keys vals)))
(my-zipmap [:a :b :c] [1 2 3])
(def v1 [:a :b :c])
(def v2 [1 2 3])
;
; Problem 62 - Implement iterate
(defn my-iter
  [f x]
  (lazy-seq
    (cons x
      (my-iter f (f x)))))
(take 5 (my-iter inc 0))

; Problem 63 - Implement group-by
; Given a function f and a sequence s, write a function which returns a map. 
; The keys should be the values of f applied to each item in s. The value at 
; each key should be a vector of corresponding items in the order they appear 
; in s.
(defn my-group-by
  [f s]
  (reduce (fn [acc element]
            (assoc acc (f element) 
                   (conj (get acc (f element) [])  ; conj the collection ithat get returns otherwise conj it to an empty vector
                         element))) 
          {} ; empty map accumulator 
          s))
(=  (my-group-by #(> % 5)  [1 3 6 8])  {false  [1 3], true  [6 8]})
(=  (my-group-by #(apply / %)  [[1 2]  [2 4]  [4 6]  [3 6]])
      {1/2  [[1 2]  [2 4]  [3 6]], 2/3  [[4 6]]})
(=  (my-group-by count  [[1]  [1 2]  [3]  [1 2 3]  [2 3]])
      {1  [[1]  [3]], 2  [[1 2]  [2 3]], 3  [[1 2 3]]})

; Problem 64 - Implement reduce
; Reduce takes a 2 argument function and an optional starting value. It then 
; applies the function to the first 2 items in the sequence (or the starting
; value and the first element of the sequence). In the next iteration the 
; function will be called on the previous return value and the next item from the 
; sequence, thus reducing the entire collection to one value. Don't worry, it's 
; not as complicated as it sounds.
(defn my-reduce
  ([f coll]
   (if-let [s (seq coll)]
     (my-reduce f (f (first s)) (rest s))
     (list (f))))
  ([f init coll]
   (when-let [s (seq coll)]
     (my-reduce f (f init (first s)) (rest s)))))
(my-reduce + '(1 2 3 4 5))
(apply + 1 2)

; Problem 67 - Prime numbers
;Write a function which returns the first x number of prime numbers.
;
(defn primes [n]
    (letfn  [(enqueue  [sieve n step]
               (let  [m  (+ n step)]
                 (if  (sieve m)
                 (recur sieve m step)
                 (assoc sieve m step))))
               (next-sieve  [sieve candidate]
                 (if-let  [step  (sieve candidate)]
                   (-> sieve
                       (dissoc candidate)
                       (enqueue candidate step))
                       (enqueue sieve candidate  (+ candidate candidate)))) 
             (next-primes  [sieve candidate]
                   (if  (sieve candidate)
                     (recur  (next-sieve sieve candidate)  (+ candidate 2))
                       (cons candidate 
                         (lazy-seq  (next-primes  (next-sieve sieve candidate) 
                                                 (+ candidate 2))))))]
      (take n (cons 2  (lazy-seq  (next-primes  {} 3))))))
(=  (primes 2)  [2 3])
(=  (primes 5)  [2 3 5 7 11])
(=  (last  (primes 100)) 541)

;my-flatten
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
;turns into recursively
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
;partition-by applies f to each value in coll, splitting it each time f returns
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
    (when (and s1 s2)
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
  (mapcat #(take (dec n) %) (partition-all n coll)))
(= (drop-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
(= (drop-nth [:a :b :c :d :e :f] 2) [:a :c :e])
(= (drop-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])
(drop-nth [1 2 3 4 5 6] 3)
(partition 4 [1 2 3 4 5 6])

; Problem 42 - factorial
(defn my-factorial [n]
  (reduce * (filter pos? (range (inc n)))))

; Problem 43 - Reverse interleave
(defn rev-interleave
  [coll n]
  (apply map list (partition n coll)))


  ; Problem 44 - Write a function which can rotate a sequence in either direction.
(fn [n coll]
  (letfn [(rotate-left [coll] (flatten 
                               (conj [] (drop 1 coll) 
                                     (take 1 coll))))
          (rotate-right [coll] (flatten 
                                (conj [] 
                                      (drop (- (count coll) 1) coll) 
                                      (take (- (count coll) 1) coll))))]
    (if (pos? n)
      (last (take (+ n 1) (iterate rotate-left coll)))
      (last (take (+ (- n) 1) (iterate rotate-right coll))))))

; Problem 46
; Write a higher-order function which flips the order of the arguments of an input function.
(defn flip 
  [f]
  ; Returns a function that takes an argument list, applies f to the reversed
  ; argument list
  (fn [& xs]
    (apply f (reverse xs))))

(= 3 ((flip nth) 2 [1 2 3 4 5]))
(= true ((flip >) 7 8))
(= 4 ((flip quot) 2 8))
(= [1 2 3] ((flip take) [1 2 3 4 5] 3))



; Problem 53
; Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. 
; If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence 
; must have a length of 2 or greater to qualify.

; partition into n sized partitions
; subsequence? return it
; otherwise partition into n - 1 sized partitions
(defn subsequence-map [coll n] (filter #(= (range (first %) (+ n (first %))) %) (partition-all n 1 coll)))

(defn longest-increasing-subsequence
  [coll]
      (loop [n (count coll)
             s (seq coll)
             result []]
        (if (not (empty? (subsequence-map s n)))
          (if (= 1 n)
            []
           (into [] (first (subsequence-map s n))))
          (recur (dec n) s result))))

(= (longest-increasing-subsequence [1 0 1 2 3 0 4 5]) [0 1 2 3])
(= (longest-increasing-subsequence [5 6 1 3 2 7]) [5 6])
(= (longest-increasing-subsequence [2 3 3 4 5]) [3 4 5])
(= (longest-increasing-subsequence [7 6 5 4]) [])

; Problem 54 - Partition a Sequence
; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(defn my-partition [n coll]
    (when-let [s (seq coll)]
      (filter #(= n (count %)) (cons (take n s) (my-partition n (drop n s))))))

(= (my-partition 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
(= (my-partition 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
(= (my-partition 3 (range 8)) '((0 1 2) (3 4 5)))


; Problem 55 - Count Occurrences
; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
(defn occ
  [coll]
  (let [id-map (group-by identity coll)]
    (zipmap (keys id-map) (map count (vals id-map)))))
; or 
(fn [xs]
  (apply hash-map
    (apply concat
      (map #(list % (count (filter #{%} xs))) (set xs)))))

#(let [instances (group-by identity %)]
   (reduce (fn [acc v] (assoc acc v 
                              (-> (instances v) 
                                   count)))
           {}
           (keys instances)))
(comp #(zipmap (keys %) (map count (vals %))) (partial group-by identity))
 
(= (occ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
(= (occ [:b :a :b :a :b]) {:a 2, :b 3})
(= (occ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

; problem 56 - Find Distinct Items
; Write a function which removes the duplicates from a sequence. 
; Order of the items must be maintained.
(defn find-distinct-items [coll] 
  (reduce (fn [acc e] 
            (if (some #(= e %) acc) 
              acc
              (conj acc e))) [] coll))

(= (find-distinct-items [1 2 1 3 1 2 4]) [1 2 3 4])
(= (find-distinct-items [:a :a :b :b :c :c]) [:a :b :c])
(= (find-distinct-items '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
(= (find-distinct-items (range 50)) (range 50))

; Problem 58, comp
(defn my-comp
  [& fns]
  (do
    (let [fs (reverse (list* fns))]
      (println fs)
      (fn [& args]
        (println args)
        (loop [ret (apply (first fs) args) fs (next fs)]
          (if fs
            (recur ((first fs) ret) (next fs))
            ret))))))



((my-comp reverse) [1 2 3 4 5])
((my-comp reverse next) [1 2 3 4 5])
((my-comp (partial + 5) (partial reduce +) take) 15 (range 50))
((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world")
(= "HELLO" ((my-comp #(.toUpperCase %) #(apply str %) take) 5 "hello world"))


; Problem 66 - Greatest Common Divisor
(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))
(= (gcd 2 4) 2)
(= (gcd 10 5) 5)
(= (gcd 5 7) 1)
(= (gcd 1023 858) 33)

; problem 67 - Prime Numbers
; Write a function which returns the first x number of prime numbers.
(defn primes [max]
  (let [enqueue (fn [sieve n factor]
                  (let [m (+ n factor)]
                    (if (sieve m)
                      (recur sieve m factor)
                      (assoc sieve m factor))))
        next-sieve (fn [sieve candidate]
                     (if-let [factor (sieve candidate)]
                       (-> sieve
                         (dissoc candidate)
                         (enqueue candidate factor))
                       (enqueue sieve candidate candidate)))]
    (vals (reduce next-sieve {} (range 2 max)))))



(defn lazy-primes3 [n]
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate 
                (lazy-seq (next-primes (next-sieve sieve candidate) 
                            (+ candidate 2))))))]
    (take n (cons 2 (lazy-seq (next-primes {} 3))))))

  (lazy-primes3 10)
 (= (primes 2) [2 3])
 (= (primes 5) [2 3 5 7 11])
 (= (last (primes 100)) 541)

 ; Problem 29 - Get the caps
 ; Write a function which takes a string and returns a new string containing only the capital letters.
(defn caps [s]
  (apply str (re-seq #"[A-Z]" s)))
 (= (caps "HeLlO, WoRlD!") "HLOWRD")
(empty? (caps "nothing"))
(= (caps "$#A(*&987Zf") "AZ")

; Problem 69 - Merge with a function
; Write a function which takes a function f and a variable number of maps. 
; Your function should return a map that consists of the rest of the maps
; conj-ed onto the first. If a key occurs in more than one map, the mapping(s) 
; from the latter (left-to-right) should be combined with the mapping in the 
; result by calling (f val-in-result val-in-latter)
(defn my-merge-with
  [f & maps]
  (letfn [(merge-two-maps [f m1 m2]
                          (reduce (fn [acc e]
                                    (let [k (first e) v (second e)]
                                      (if (acc k)
                                        (assoc acc k (f (acc k) v))
                                        (assoc acc k v)))) m1 m2))]
    (reduce (fn [acc m] (merge-two-maps f acc m)) {} maps))) 
      
(defn merge2maps 
  [f m1 m2]
  (reduce (fn [acc e]
            (let [k (first e)
                  v (second e)]
              (if (acc k)
                (assoc acc k (f (acc k) v))
                (assoc acc k v)))) m1 m2))

(merge2maps - {1 10, 2 20} {1 3, 2 10, 3 15})
(def mappyq {1 10 2 20})
(my-merge-with * {:a 2 :b 3 :c 4} {:a 2 :b 3 :c 4})
(= (my-merge-with * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20})
(= (my-merge-with - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15})
(= (my-merge-with concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]})



(fn [f & x] ((comp #(zipmap (keys %) 
                            (map (comp (partial reduce f) (partial map last)) 
                                 (vals %))) 
                   (partial group-by first) 
                   (partial apply concat)) 
              x))



(def p0 (apply concat '({:a 2 :b 3 :c 4} {:a 2} {:b 2})))
(map (partial map last) (vals (group-by first (apply concat '({:a 2 :b 3 :c 4} {:a 2} {:b 2} {:c 5}))))) ; maps last on the individual subvector in each vector 
(map last (vals (group-by first (apply concat '({:a 2 :b 3 :c 4} {:a 2} {:b 2} {:c 5})))))
(def p1 (group-by first (apply concat '({:a 2 :b 3 :c 4} {:a 2} {:b 2} {:c 5}))))
(def pv (vals p1))
((partial apply concat) '({:a 2 :b 3 :c 4} {:a 2} {:b 2}))
(map (comp (partial reduce *) (partial map last)) pv)

(map (partial map list) [[1 2] [3 4] [5 6] [7 8]])
(map list [1 2] [3 4] [5 6])
(map list [1 2])



; Problem 70 - Word sorting
; Write a function that splits a sentence up into a sorted list of words. 
; Capitalization should not affect sort order and punctuation should be ignored.
(defn word-sort
  [s]
  (sort-by #(clojure.string/lower-case %) (map (partial apply str) 
                                            (filter #(not= '(\space) %) 
                                                    (partition-by #(= % \space) 
                                                                 (remove #{\. \, \! \?} (seq s)))))))
; threading macro
(defn word-sort2
  [s]
  (->> (seq s)
    (remove #{\. \, \! \?})
    (partition-by #(= % \space))
    (filter #(not= '(\space) %))
    (map (partial apply str))
    (sort-by clojure.string/lower-case)))

; or, more succinctly
(sort-by #(.toLowerCase %) (re-seq #"\w+" "Have a nice day."))

(= (word-sort2  "Have a nice day.")
   ["a" "day" "Have" "nice"])
(= (word-sort  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])
(= (word-sort  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])


; Problem 73 - Analyze a Tic-Tac-Toe Board
; A tic-tac-toe board is represented by a two dimensional vector. X is 
; represented by :x, O is represented by :o, and empty is represented by :e. 
; A player wins by placing three Xs or three Os in a horizontal, vertical, 
; or diagonal row. Write a function which analyzes a tic-tac-toe board and 
; returns :x if X has won, :o if O has won, and nil if neither player has won.
(defn tic-tac-toe
  [board]
  (do
    (let [[[tl tm tr] [ml mm mr] [bl bm br] :as b] board
          v1 [tl ml bl]
          v2 [tm mm bm]
          v3 [tr mr br]
          h1 [tl tm tr]
          h2 [ml mm mr]
          h3 [bl bm br]
          d1 [tl mm br]
          d2 [tr mm bl]
          wins (map (partial reduce (fn [a b] (if (= a b) a nil))) 
                    [v1 v2 v3 h1 h2 h3 d1 d2])]
    (cond 
      (every? #(= :e %) b) nil
      (some #(= :x %) wins) :x
      (some #(= :o %) wins) :o
      :else nil))))

(= :e :e)
(= nil (tic-tac-toe [[:e :e :e]
                     [:e :e :e]
                     [:e :e :e]]))
(= :x (tic-tac-toe [[:x :e :o]
                    [:x :e :e]
                    [:x :e :o]]))
(= :o (tic-tac-toe [[:e :x :e]
                    [:o :o :o]
                    [:x :e :x]]))
(= nil (tic-tac-toe [[:x :e :o]
                     [:x :x :e]
                     [:o :x :o]]))
(= :x (tic-tac-toe [[:x :e :e]
                    [:o :x :e]
                    [:o :e :x]]))
(= :o (tic-tac-toe [[:x :e :o]
                    [:x :o :e]
                    [:o :e :x]]))
(= nil (tic-tac-toe [[:x :o :x]
                     [:x :o :x]
                     [:o :x :o]]))

; Problem 74 - Filter Perfect Squares
; Given a string of comma separated integers, write a function which returns a
; new comma separated string that only contains the numbers which are perfect 
; squares.
(defn filter-perfect
  [s]
  (let [perfect-squares (lazy-seq
                          (map #(* % %) (range)))
        perfect-square? (fn [n] (boolean (some #(= % n) (take n perfect-squares))))]
    (apply str (interpose "," (filter perfect-square?
                                      (map (comp (fn [a] (Integer. a)) str) (remove #{\,} (seq s))))))))
(map )
(def perfect-squares
  (lazy-seq
    (map #(* % %) (range))))
(defn perfect-square?
  [n]
  (boolean (some #(= % n) (take n perfect-squares))))
(defn testperf
  [s]
 (apply str (interpose "," (filter perfect-square?  
  (map (comp  (fn [a] (Integer. a)) str) (remove #{\,} (seq s)))))))
(testperf "4,5,6,7")
(perfect-square? 10)
 (map (fn [a] (Integer. a)) ["4" "5"])
(int (str \4))
(Integer/parseInt "4")
(=  (filter-perfect "4,5,6,7,8,9") "4,9")
(=  (filter-perfect "15,16,25,36,37") "16,25,36")
(filter perfect-square? (map (comp (fn [a] (Integer. a)) str) (remove #{\,} (seq "14,15,16,17,18"))))
