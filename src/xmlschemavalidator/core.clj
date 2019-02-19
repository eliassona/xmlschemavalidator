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
    #{:name} `[~(:name attrs) (fn [~'value] ~@content)]
    #{:name :type} `[~(:name attrs)  (fn [~'value] ((~'type-map ~(:type attrs)) ~@content))]
    #{} (fn-of (apply-of (first content)))
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
  `(when-let [type# ~(:type attrs)]
     ((deref ~'type-map) type#)))

(defn parse-element [attrs content]
 (with-meta `(fn [~'value] ~(element-of attrs content)) attrs))

(defn parse-union 
  [attrs content]
    `~(add-try-catch (map add-type-map (.split (:memberTypes attrs) " "))))


(defn elem->name [e] (-> e :attrs :name keyword))

(defn validate-element [value elements type-map]
  (-> (filter #(= (:tag value) (elem->name %)) elements) first value))

(defn add-swap! [[n v]]
  `(swap! ~'type-map assoc ~n ~v))

(defn element? [v] (if (nil? (meta v)) false true))

(defn parse-schema [attrs content] 
  `(defn ~'decode [~'value] 
     (let [~'type-map (atom {})]
       ~@(map add-swap! (partition 2 (reduce concat (filter vector? content))))
       (validate-element ~'value ~(filter element? content) (deref ~'type-map))
       )))

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
