(ns xmlschemavalidator.core
  (:use [clojure.pprint])
  (:require [clojure.data.xml :refer [parse-str parse]]
            [clojure.spec.alpha :as s])
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

(defn parse-element [the-map element]
  (if-let [f (the-map (:tag element))]
    (f (:attrs element) (transform the-map (:content element)))
    (assoc element :content (transform the-map (:content element)))))  

(defn transform [the-map schema] 
  (cond
    (map? schema) (parse-element the-map schema)
    (string? schema) schema
    :else
    (map (partial transform the-map) schema)))

(defn parse-simple-type [attrs content]
  (condp = (.keySet attrs)
    #{:name} `(let [~'_ (reset! ~'type-map ~(:name attrs) (fn [~'value] ~@content))
                    ~'parent-type-map ~'type-map
                    ~'type-map (atom {})])
    #{:name :type} content
    #{} content
    ))
    


(defn parse-str-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value)))
(defn parse-int-attr [op attrs content] (parse-str-attr op attrs content))

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

(defn parse-union 
  ([attrs content]
  
  )
  ([fns]
    ))

(def parse-map 
  {:simpleType parse-simple-type
   :restriction parse-restriction
   :union parse-union,
   :schema (fn [attrs content] `(defn decode [~'value] (let [~'type-map (atom {})] ~content)))})



