(ns xmlschemavalidator.core
  (:use [clojure.pprint])
  (:require [clojure.data.xml :refer [parse-str parse]]
            [clojure.spec.alpha :as s]
            [clojure.string :refer [split]])
  (:import [java.io ByteArrayInputStream InputStream])
  )

(require '[clojure.spec.alpha :as s])

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn fn-of [expr]
  `(fn [~'value ~'env] ~expr))
(defn first-fn-of [expr]
  `(fn [[~'value] ~'env] ~expr))

(defn apply-of [expr]
  `(~expr ~'value ~'env))

(defn error [msg]
  (throw (IllegalArgumentException. msg)))


(declare transform)
(declare parse-node)
  

(defn transform [the-map schema] 
  (cond
    (map? schema) (parse-node the-map schema)
    (string? schema) schema
    :else
    (map (partial transform the-map) schema)))


(defn parse-node [the-map element]
  (if-let [f (the-map (:tag element))]
    (f (:attrs element) (transform the-map (:content element)))
    (assoc element :content (transform the-map (:content element)))))

(defn parse-simple-type [attrs content]
  (condp = (.keySet attrs)
    #{:name} `[~(:name attrs) (fn-of ~@content)]
    #{:name :type} (with-meta [(:name attrs) (first-fn-of `((~'env ~(:type attrs) ~@content) ~'value ~'env))] {:kind :type})
    #{} (first-fn-of (apply-of (first content)))
    ))
    


(defn parse-str-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value)))
(defn parse-int-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value read-string)))

(def restriction-map 
  {:minInclusive (partial parse-int-attr '>=),
   :maxInclusive (partial parse-int-attr '<=)
   :enumeration (partial parse-str-attr '=)})

(defn enumeration? [content]
  (every? #(= (:tag %) :enumeration) content))

(defn parse-restriction [attrs content]
    (fn-of 
      (condp = (:base attrs)
        "integer"
        (if (enumeration? content)
          `(or ~@(transform restriction-map content))
          `(and ~@(transform restriction-map content)))
        "string"
        `(or ~@(transform restriction-map content))
         )))

(defn add-type-map [member]
  `(((deref ~'type-map) ~member) ~'value)
  )

(defn add-try-catch [unions]
  (if (empty? (rest unions))
    `~(first unions)
    `(try ~(first unions) (catch Exception e# ~@(rest unions)))))
  
(defn element-of [attrs content]
  (apply-of `(~'env ~(:type attrs))))

(defn parse-element [attrs content]
 (with-meta {(-> attrs :name keyword) (fn-of (element-of attrs content))} {:kind :element}))

(defn parse-union 
  [attrs content]
    `~(add-try-catch (map add-type-map (.split (:memberTypes attrs) " "))))


(defn elem->name [e] (-> e :attrs :name keyword))

(defn validate-element [value elements type-map]
  (-> (filter #(= (:tag value) (elem->name %)) elements) first value))

(defn add-swap! [[n v]]
  `(swap! ~'type-map assoc ~n ~v))

(defn element? [v] (= (-> v meta :kind) :element))

(defn parse-schema [attrs content]
  (let [elements (filter element? content)]
    (fn-of `((~(apply merge elements) (:tag ~'value)) (:content ~'value) ~'env))))

(defn parse-sequence [attrs content]
  `(fn [~'value] ~(map elem->name (dbg content))))

(def parse-map 
  {:simpleType parse-simple-type
   :restriction parse-restriction
   :union parse-union,
   :sequence parse-sequence
   :schema parse-schema
   :element parse-element
   :complexType parse-element})

(def allowed (fn [value _] true))

(def predef-env
  {
   "string" allowed
   "float" allowed
   "double" allowed
   "decimal" allowed
   "positiveInteger" (fn [value _] (> value 0))
   "negativeInteger" (fn [value _] (< value 0))
   "nonPositiveInteger" (fn [value _] (<= value 0))
   "nonNegativeInteger" (fn [value _] (>= value 0))
   "long" (fn [value _] (and (<= value 9223372036854775807) (>= value -9223372036854775808)))
   "int" (fn [value _] (and (<= value 2147483647) (>= value -2147483648)))
   "short" (fn [value _] (and (<= value 32767) (>= value -32768)))
   "byte" (fn [value _] (and (<= value 127) (>= value -128)))
   "unsignedLong" (fn [value _] (and (<= value 18446744073709551615) (>= value 0)))
   "unsignedInt" (fn [value _] (and (<= value 4294967295) (>= value 0)))
   "unsignedShort" (fn [value _] (and (<= value 65535) (>= value 0)))
   "unsignedVByte" (fn [value _] (and (<= value 255) (>= value 0)))
   }
  )

(defn validation-expr-of [element]
  (transform parse-map (parse-str element)))

(defn validation-fn-of [element]
  (eval (validation-expr-of element)))

(defmacro def-schema [schema]
  (transform parse-map (parse-str schema)))





(comment 
  (pprint (transform parse-map (parse-str schema)))
  ((((eval (transform parse-map (parse-str schema))) nil) "intrange") 37)
  )
