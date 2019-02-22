(ns xmlschemavalidator.core
  (:use [clojure.pprint])
  (:require [clojure.data.xml :refer [parse-str parse]])
  (:import [java.io ByteArrayInputStream InputStream]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn fn-of [expr] `(fn [~'value ~'env] ~expr))

(defn apply-of [expr] `(~expr ~'value ~'env))

(defn error [msg] (throw (IllegalArgumentException. msg)))

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
    #{:name} (with-meta {(:name attrs) (fn-of (apply-of (first content)))} {:kind :type})
    #{:name :type} (with-meta {(:name attrs) (fn-of `((~'env ~(:type attrs) ~@content) ~'value ~'env))} {:kind :type})
    #{} (-> content first apply-of fn-of)))
    
(defn parse-str-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value)))
(defn parse-int-attr [op attrs _]
  `(~op ~'value ~(-> attrs :value read-string)))

(defn parse-pattern [attrs _]
  `(.matches ~'value ~(-> attrs :value str)))

(defn parse-str-length [op attrs _]
  `(~op (count ~'value) ~(-> attrs :value read-string)))



(def restriction-map 
  {:minInclusive (partial parse-int-attr '>=),
   :maxInclusive (partial parse-int-attr '<=)
   :enumeration (partial parse-str-attr '=)
   :pattern parse-pattern
   :minLength (partial parse-str-length >=)
   :maxLength (partial parse-str-length <=)})

(defn enumeration? [content]
  (every? #(= (:tag %) :enumeration) content))

(defn parse-restriction [attrs content]
  (fn-of 
    `(and
       ~(apply-of `(~'env ~(:base attrs)))
       ~(condp = (:base attrs)
         "integer"
         (if (enumeration? content)
           `(or ~@(transform restriction-map content))
           `(and ~@(transform restriction-map content)))
         "string"
         `(or ~@(transform restriction-map content))
          )
       )))

(defn add-type-map [member]
  `(((deref ~'type-map) ~member) ~'value)
  )
(defn throw-if-false [b] (if b b (throw (IllegalArgumentException.))))

(defn add-try-catch [unions]
  (if (empty? (rest unions))
    `~(first unions)
    `(try (throw-if-false ~(first unions)) (catch Exception e# ~(add-try-catch (rest unions))))))
  
(defn element-of [attrs content]
  (apply-of `(~'env ~(:type attrs))))

(defn parse-element [attrs content]
 (with-meta {(-> attrs :name keyword) (fn-of (element-of attrs content))} {:kind :element}))

(defn member-types-of [member-types]
  (map (fn [m] (apply-of `(~'env ~m))) (.split member-types " ")))

(defn anom-types-of [types]
  (map apply-of types))

(defn parse-union 
  [attrs content]
    (fn-of 
      `~(add-try-catch 
          (concat
            (member-types-of (:memberTypes attrs))
            (anom-types-of content)))))

(defn elem->name [e] (-> e :attrs :name keyword))

(defn kind [v] (-> v meta :kind))

(defn element? [v] (= (kind v) :element))

(defn type? [v] (= (kind v) :type))

(defn simple-type? [value]
  (and (= (count value) 1) (not (map? (first value)))))

(defn to-str [v] (if (symbol? v) (str v) v))

(defn content-of [value]
  (let [value (:content value)]
    (if (simple-type? value)
      (-> value first read-string to-str)
      value)))

(defn parse-schema [attrs content]
  (let [types (filter type? content)
        elements (filter element? content)]
    (fn-of 
      `(let [~'env (merge ~(apply merge types) ~'env)
             elems# ~(apply merge elements)]
         ((elems# (:tag ~'value)) (content-of ~'value) ~'env)))))


(defn parse-sequence [attrs content]
  (fn-of 
    `(let [~'elem-map ~(apply merge content)]
        (if
          (= (keys ~'elem-map) (map :tag ~'value))
          (every? identity (map (fn [v#] ((~'elem-map (:tag v#)) (content-of v#) ~'env)) ~'value))
          false))))

(defn parse-choice [attrs content]
  (fn-of 
    `(let [~'elem-map ~(apply merge content)]
        (if
          (and (= (count ~'value) 1)
               (contains? (.keySet ~'elem-map) (-> ~'value first :tag)))
          (every? identity (map (fn [v#] ((~'elem-map (:tag v#)) (content-of v#) ~'env)) ~'value))
          false))))

(defn parse-all [attrs content]
  (fn-of 
    `(let [~'elem-map ~(apply merge content)]
        (if
          (and 
            (= (count ~'elem-map) (count ~'value))
            (= (.keySet ~'elem-map) (set (map :tag ~'value))))
          (every? identity (map (fn [v#] ((~'elem-map (:tag v#)) (content-of v#) ~'env)) ~'value))
          false))))

(defn parse-complex-type [attrs content]
  (fn-of attrs))

(def parse-map 
  {:simpleType parse-simple-type
   :restriction parse-restriction
   :union parse-union,
   :sequence parse-sequence
   :choice parse-choice
   :schema parse-schema
   :element parse-element
   :complexType parse-simple-type
   :all parse-all})

(def allowed (fn [value _] true))

(def numeric? (fn [value _] (number? value)))

(defmacro def-base [expr base]
  `(fn [~'value ~'env] (and ((~'env ~base) ~'value ~'env) ~expr)))

(def predef-env
  {
   "string" (fn [value _] (string? value))
   "float" numeric?
   "double" numeric?
   "decimal" numeric?
   "integer" numeric?
   "positiveInteger" (def-base (> value 0) "integer")
   "negativeInteger" (def-base (< value 0) "integer")
   "nonPositiveInteger" (def-base (<= value 0) "integer")
   "nonNegativeInteger" (def-base (>= value 0) "integer")
   "long" (def-base (and (<= value 9223372036854775807) (>= value -9223372036854775808)) "integer")
   "int" (def-base (and (<= value 2147483647) (>= value -2147483648)) "integer")
   "short" (def-base (and (<= value 32767) (>= value -32768)) "integer")
   "byte" (def-base (and (<= value 127) (>= value -128)) "integer")
   "unsignedLong" (def-base (and (<= value 18446744073709551615) (>= value 0)) "integer")
   "unsignedInt" (def-base (and (<= value 4294967295) (>= value 0)) "unsignedLong")
   "unsignedShort" (def-base (and (<= value 65535) (>= value 0)) "unsignedInt")
   "unsignedByte" (def-base (and (<= value 255) (>= value 0)) "unsignedShort")
   }
  )

(defn validation-expr-of [element]
  (transform parse-map (parse-str element)))

(defn validation-fn-of [element]
  (eval (validation-expr-of element)))



