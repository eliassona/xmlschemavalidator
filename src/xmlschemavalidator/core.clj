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
    #{:name} `[~(:name attrs) (fn [~'value] ~@content)]
    #{:name :type} `[~(:name attrs)  (fn [~'value] ((~'type-map ~(:type attrs)) ~@content))]
    #{} `(fn [~'value] ~@content)
    ))
    


(defn parse-str-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value)))
(defn parse-int-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value read-string)))

(def restriction-map 
  {:minInclusive (partial parse-int-attr '<=),
   :maxInclusive (partial parse-int-attr '>=)
   :enumeration (partial parse-str-attr '=)})

(defn enumeration? [content]
  (every? #(= (:tag %) :enumeration) content))

(defn parse-restriction [attrs content]
  (condp = (:base attrs)
    "integer"
    (if (enumeration? content)
      `(or ~@(transform restriction-map content))
      `(and ~@(transform restriction-map content)))
    "string"
    `(or ~@(transform restriction-map content))
     )
  )

(defn add-type-map [member]
  `((~'type-map ~member) ~'value)
  )

(defn add-try-catch [unions]
  (if (empty? (rest unions))
    `~(first unions)
    `(try ~(first unions) (catch Exception e ~@(rest unions)))))
  

(defn parse-union 
  [attrs content]
    `~(add-try-catch (map add-type-map (.split (:memberTypes attrs) " "))))

(defn validate-schema [value elements type-map]
  )

(defn parse-schema [attrs content] 
  `(defn ~'decode [~'value] (let [~'type-map ~(apply hash-map (reduce concat (filter vector? content)))])))

(def parse-map 
  {:simpleType parse-simple-type
   :restriction parse-restriction
   :union parse-union,
   :schema parse-schema})



