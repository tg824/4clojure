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
