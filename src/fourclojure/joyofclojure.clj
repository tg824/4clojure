(ns fourclojure.joyofclojure)
;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(interleave [1 2 3] [4 5 6])

(do
  6
  (+ 5 4)
  3)

(let [r   5
      pi  3.1415
      r-squared (* r r)]
  (println "radius is " r)
  (* pi r-squared))

(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x))))

(print-down-from 5)

(defn sum-down-from [sum x]
  (if (pos? x)
    (recur (+ sum x) (dec x))
    sum))
(sum-down-from 0 2)

(defn sum-down-from-loop [initial-x]
  (loop [sum 0, x initial-x]
    (if (pos? x)
      (recur (+ sum x) (dec x))
      sum)))
(sum-down-from-loop 10)

1
(def tena 9)
(quote tena)
(cons 1 '(2 3))

; syntax quote
(str `map)
(str `(+ 10 (* 3 2)))

; syntax quote using unquote to evaluate (* 3 2)
(str `(+ 10 ~(* 3 2)))

; unquote
(let [x '(2 3)] `(1 ~x))
; unquote splicing - splice into existing list
(let [x '(2 3)] `(1 ~@x))

; auto-gensym
; Sometimes you need an unqualified symbol, such as for a parameter or
; let local name. The easiest way to do this inside a syntax-quote is
; to append a # to the symbol name. This will cause Clojure to generate
; a new unqualified symbol:
(str `potion#)

; interop
java.util.Locale/JAPAN
; java.lang.Math#sqrt static method
(Math/sqrt 9)
; Creating java class instances
(new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"})
; more idiomatic
(java.util.HashMap. {"foo" 42 "bar" 9 "baz" "quux"})

; Accessing Java instance members with the . operator
(.x (java.awt.Point. 10 20))
(.divide (java.math.BigDecimal. "42") 2M)

; Setting Java instance properties
'(let [origin (java.awt.Point. 0 0)]
  (set! (.x origin) 15)
  (str (.x origin)))

; That should set x to 15 there's something that LightTable doesn't like about it

; The .. macro
(.endsWith (.toString (java.util.Date.)) "2013")
; equivalent to
(.. (java.util.Date.) toString (endsWith "2013"))

; The doto macro
; typically do set of mutators to fresh instance in Java
; java.util.HashMap props = new java.util.HashMap(); /* More java code. */
; props.put("HOME", "/home/me"); /* Sorry. */
; props.put("SRC", "src");
; props.put("BIN", "classes");

(doto (java.util.HashMap.)
(.put "HOME" "/home/me")
(.put "SRC" "src")
(.put "BIN" "classes"))

; Exceptions
'(throw (Exception. "I done throwed"))

; try catch
(defn throw-catch [f]
  [(try
     (f)
   (catch ArithmeticException e "No dividing by zero!")
   (catch Exception e (str "You are so bad " (.getMessage e)))
   (finally (println "returning...")))])

(throw-catch #(/ 10 15))
(throw-catch #(/ 10 0))
(throw-catch #(throw (Exception. "foo")))

; Namespaces
(ns insta.core)
; Shows current namespace
*ns*
(defn report-ns [] (str "The current namespace is " *ns*))
(report-ns)

(ns insta.core2)
'(report-ns) ; Does not work because we switched namespaces

; Loading other namespaces with :require
(ns joy.req
  (:require clojure.set))
(clojure.set/intersection #{1 2 3} #{3 4 5})

; Using :require indicates that you want the clojure.set namespace loaded,
; but you don’t want the mappings of symbols to functions in the joy.req
; namespace. You can also use the :as directive to create an additional
; alias to clojure.set:

(ns joy.req-alias
  (:require [clojure.set :as s]))
(s/intersection #{1 2 3} #{3 4 5})

; Loading and creating mappings with :use
; Don't need to use qualifying namespace symbol
(ns joy.use-ex
  (:use [clojure.string :only [capitalize]]))
(map capitalize ["kilgore" "trout"])

; exclude names
; '(ns joy.exclusion
;   (:use [clojure.string :exclude [capitalize]]))

; Creating mappings with :refer
; works like :use except it only creates mappings for libraries that
; have already been loaded
'(ns joy.yet-another
  (:refer insta.core))
'(report-ns)


; Loading Java classes with :import
(ns joy.java
  (:import [java.util HashMap]
           [java.util.concurrent.atomic AtomicLong]))
(HashMap. {"happy?" true})
(AtomicLong. 42)

; Truthiness
; Every value looks like true to 'if' except for false and nil
; zero-length strings, empty lists, the number zero, and so on—are
; all treated as true in Clojure
; Every object is true all the time, unless it's nil or false
(if true :truthy :falsey)
(if [] :truthy :falsey)
(if nil :truthy :falsey)
(if false :truthy :falsey)

; This will be true
(def evil-false (Boolean. "false")) ; NEVER do this
evil-false
(= false evil-false)
; but it's still true
(if evil-false :truthy :falsey)
; The right way
(if (Boolean/valueOf "False") :truthy :falsey)

; nil versus false
(when (nil? nil) "Actually nil, not false")

;nil punning
(seq [1 2 3])
(seq [])
; seq returns a sequence view of a collection, or nil if the collection is empty
; used for determining loop termination
; better to use than when-not (empty? ) because [] is true
(if [] :truthy :falsey)
(defn print-seq [s]
  (when (seq s)
    (prn (first s))
         (recur (rest s))))
(print-seq [1 2])
; prefer doseq when you are going to have side-effects

; Destructuring
; allows you to positionally bind locals based on an expected form for a composite
; data structure

; Take a vector of length 3 that represents a person’s first, middle, and
; last names and return a string that will sort in the normal way, like
; “Steele, Guy Lewis”.

(def guys-whole-name ["Guy" "Lewis" "Steele"])
(str (nth guys-whole-name 2) ", "
     (nth guys-whole-name 0) " "
     (nth guys-whole-name 1))

; destructuring with a vector
(let [[f-name m-name l-name] guys-whole-name]
  (str l-name ", " f-name " " m-name))

; can use & for rest-args
(let [[a b c & more] (range 10)]
  (println "a b c are: " a b c)
  (println "more is:" more))

; :as can bind a local to the entire collection. must be placed after the & local if there is one
(let [range-vec (vec (range 10))
      [a b c & more :as all] range-vec]
  (println "a b c are:" a b c)
  (println "more is:" more)
  (println "all is:" all))
; note all stays as a vector, more is a seq

; Destructuring with a map
(def guys-name-map
  {:f-name "Guy" :m-name "Lewis" :l-name "Steele"})
(let [{f-name :f-name, m-name :m-name, l-name :l-name} guys-name-map]
  (str l-name ", " f-name " " m-name))

; use :keys feature for repetitiveness
(let [{:keys [f-name m-name l-name]} guys-name-map]
  (str l-name ", " f-name " " m-name))

; So by using :keys instead of a binding form, we’re telling Clojure that the next form
; will be a vector of names that it should convert to keywords such as :f-name in order
; to look up their values in the input map. Similarly, if we had used :strs, Clojure
; would be looking for items in the map with string keys such as "f-name", and :syms
; would indicate symbol keys.
;      The directives :keys, :strs, :syms, and regular named bindings can appear in
; any combination and in any order. But sometimes you’ll want to get at the original
; map—in other words, the keys that you didn’t name individually by any of the methods
; just described. For that, you want :as, which works just like it does with vector destructuring:

(let [{f-name :f-name, :as whole-name} guys-name-map]
  whole-name)

;If the destructuring map looks up a key that’s not in the source map, it’s normally
; bound to nil, but you can provide different defaults with :or:

(let [{:keys [title f-name m-name l-name], :or {title "Mr."}} guys-name-map]
(println title f-name m-name l-name))

; Associative destructuring
; map declaring the local name as indices
(let [{first-thing 0, last-thing 3} [1 2 3 4]]
  [first-thing last-thing])

; Destructuring in function parameters
(defn print-last-name [{:keys [l-name]}]
  str l-name)
(print-last-name guys-name-map)

(defn xors [max-x max-y] (for [x (range max-x) y (range max-y)]
                           [x y (bit-xor x y)]))

(xors 2 2)

; Find methods with regular expression #"Vis"
(for [method (seq (.getMethods java.awt.Frame))
      :let [method-name (.getName method)]
      :when (re-find #"Vis" method-name)]
  method-name)

; Truncation example
(let [imadeuapi 3.14159265358979323846264338327950288419716939937M]
 (println (class imadeuapi))
  imadeuapi)

(let [butieatedit 3.14159265358979323846264338327950288419716939937]
  (println (class butieatedit))
    butieatedit)

; Promotion - looks like they switched from Integer to Long
(def clueless 9)
(class clueless)
(class (+ clueless 9000000000000000))
(class (+ clueless 90000000000000000000))
(class (+ clueless 9.0))

; Overflow -- doesn't really work
(+ Integer/MAX_VALUE Integer/MAX_VALUE)

; Underflow
(float 0.0000000000000000000000000000000000000000000001)
1.0E-430

; Rounding errors
(let [aprox-interval (/ 209715 2097152)
      actual-interval (/ 1 10)
      hours (* 3600 100 10)
      actual-total (double (* hours actual-interval))
      aprox-total (double (* hours aprox-interval))]
  (- actual-total aprox-total))
 
; In Clojure, any computation involving even a single double will result in a 
; value that's a double
(+ 0.1M 0.1M 0.1M 0.1 0.1M 0.1M 0.1M 0.1M 0.1M 0.1M)
; Weird floating point stuff and it's not associative or distributive
(def a 1.0e50)
(def b -1.0e50)
(def c 17.0e00)
(+ (+ a b) c) ; Associativity should guarantee 17.0 also
(+ a (+ b c))
 
(let [a (float 0.1)
      b (float 0.2)
      c (float 0.3)]
  (=  ; not equal
    (* a (+ b c))
    (+ (* a b)) (* a c)))
; For absolutely precise calculations, rationals are the best choice
(def a (rationalize 1.0e50))
(def b (rationalize -1.0e50))
(def c (rationalize 17.0e00))
(+ (+ a b) c)
(+ a (+ b c)) ; Associativity preserved

;Distributivity preserved
(let [a (rationalize 0.1)
      b (rationalize 0.2)
      c (rationalize 0.3)]
  (= 
   (* a (+ b c))
   (+ (* a b) (* a c))))

; To ensure that your numbers remain rational, you can use rational? to check 
; whether a given number is one and then use rationalize to convert it to one. There 
; are a few rules of thumb to remember if you want to maintain perfect accuracy in your 
; computations: 
; 1 Never use Java math libraries unless they return results of BigDecimal, and even 
; then be suspicious. 
; 2 Don't rationalize values that are Java float or double primitives. 
; 3 If you must write your own high-precision calculations, do so with rationals. 
; 4 Only convert to a floating-point representation as a last resort. 
; Finally, you can extract the constituent parts of a rational using the numerator and 
; denominator functions: 
(numerator (/ 123 10))
(denominator (/ 123 10))


; When to use Keywords
; Keywords always refer to themselves
(eval :magma)
;They are truthy
(if :magma :truthy :falsey)
; Good for keys
(def population {:zombies 2700 :humans 9})
(:zombies population)
(println (/ (:zombies population)
            (:humans population))
         "zombies per capita")
; Enumerations
(def sizes [:small :medium :large])
; as directives t0 functions
(defn pour
  [lb ub]
  (cond
    (= ub :tojours) (iterate inc lb)
    :else (range lb ub)))
(pour 1 10)
(pour 1 :tojours)

; Keywords don't belong to any namespace
(defn do-blowfish [directive]
  (case directive
    :aquarium/blowfish (println "feed the fish")
    :crypto/blowfish (println "encode the message")
    :blowfish (println "not sure what to do")))
(comment 
  (ns crypto)
  (fourclojure.joyofclojure/do-blowfish :blowfish)
  (fourclojure.joyofclojure/do-blowfish ::blowfish)
  (ns aquarium)
  (fourclojure.joyofclojure/do-blowfish :blowfish)
  (fourclojure.joyofclojure/do-blowfish :blowfish))
;
; Keywords vs Symbols
(identical? :goat :goat)
(identical? 'goat 'goat)

(= 'goat 'goat)
(+ 2 2)
[1 2 3 4 5]
(vec (range 10))

; Already have a vector but want to "pour" several values into it, use into
(let [my-vector [:a :b :c]]
  (into my-vector (range 10)))

; primitive vectors stores contents as primitives internally
; will coerce additions into its internal type
(into (vector-of :int) [Math/PI 2 1.3])
(into (vector-of :char) [100 101 102])
;(into (vector-of :int) [1 2 62387637126781267326786327863]) throws IllegalArgumentException: Value out of range for int

(def a-to-j (vec (map char (range 65 75))))
; All of these do the same work and return \E
(nth a-to-j 4)
(get a-to-j 4)
(a-to-j 4)

; Vectors can be walked in either direction
(seq a-to-j)
(rseq a-to-j)

; Any item in a vector can be changed using the assoc function
(assoc a-to-j 4 "no longer E")
; assoc only works for vectors on indices that already exist in the vector, or one step past the end
; replace and assoc work on seqs and vectors, but replace uses assoc whem given a vector
(replace {2 :a 4 :b} [1 2 3 2 3 4])

; assoc-in and update-in are for working with nested structures of vectors and/or maps
(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(get-in matrix [1 2])
(get-in matrix [2])
(get-in matrix [2 2])

(assoc-in matrix [1 2] 'x)

;update-in takes a function to apply to an existing value, instead of overwriting it
(update-in matrix [1 2] * 100)


; a function for finding the neighbors of a spot on a 2D matrix
(defn neighbors
  ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (fn [new-yx]
           (every? #(< -1 % size) new-yx))
         (map #(map + yx %) deltas))))

(map #(get-in matrix %) (neighbors 3 [0 0]))

; Vectors as stacks
; ; conj pushes onto the right sde, pop pops off the right side
(def my-stack [1 2 3])
(conj my-stack 4)
; notice the persistance
(peek my-stack)
(pop my-stack)
(+ (peek my-stack) (peek (pop my-stack)))
; conj, pop, and peek work on any object that implements clojure.lang.IPersistentStack
;
; idiomatic clojure code does not use reverse.  Consider using a vector as an accumulator.
; requires reverse
(defn strict-map
  [f coll]
  (loop [coll coll, acc nil]
    (if (empty? coll)
      (reverse acc)
      (recur (next coll) (cons (f (first coll)) acc)))))
(strict-map - (range 5))

; does not require reverse
(defn strict-map2
  [f coll]
  (loop [coll coll, acc []] 
    (if (empty? coll)
      acc
      (recur (next coll) (conj acc (f (first coll)))))))
(strict-map2 - (range 5))

; subvectors
(subvec a-to-j 3 6)
; Vectors as MapEntries
(first {:width 10, :height 20,  :depth 15})
; it's a vector
(vector? (first {:width 10 :height 20 :depth 15}))
; can use conj, get on map entries, can destructure
(doseq [[dimension amount] {:width 10 :height 20 :depth 15}]
  (println (str (name dimension) ":") amount  "inches"))
; Lists
; in idiomatic clojure, lists are used to represent code forms
;
; cons and conj act differently
(cons 1 '(2 3))
(conj '(2 3) 1)
; the 'right' way is to use conj, because it is more efficient
; conj also guarantees that it will be a list, cons only guarantees
; it will be a seq.  Use cons on lay seqs, a range or any other type of
; seq.  If you want it to definitely be a list, use conj.

; lists can also be used as stacks
(pop '(1 2 3))
(peek '(1 2 3))
(conj '(1 2 3) 4)

; Queues
(defmethod print-method clojure.lang.PersistentQueue 
  [q, w]
   (print-method '<- w)  (print-method  (seq q) w)  (print-method '-< w))
(def schedule
  (conj clojure.lang.PersistentQueue/EMPTY
        :wake-up :shower :brush-teeth)) 
(print schedule)
(peek schedule)
(pop schedule)
(rest schedule)

; sets - collection of unique unsorted elements
; sets are functions of their elements that return the matched element or nil
(#{:a :b :c :d} :c)
(#{:a :b :c :d} :e)
; can be queried using get
(get #{:a :b :c :d} :a)
(get #{:a 1 :B 2} :nothing)
; sorted sets need to be comparable
(sorted-set 2 3 1)
(sorted-set [3 4] [1 2])
(sorted-set b 2 :c a 3 1)

; contains is weird
(contains? #{1 2 4 5} 4)
(contains? [1 2 4 3] 4)
