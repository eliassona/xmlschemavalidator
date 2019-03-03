(ns xmlschemavalidator.core
  (:use [clojure.pprint])
  (:require [clojure.data.xml :refer [parse-str parse sexp-as-element]])
  (:import [java.io ByteArrayInputStream InputStream]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn fn-of [expr] `(fn [~'value ~'types ~'attr-groups ~'elements] ~expr))

(defn apply-of [expr] `(~expr ~'value ~'types ~'attr-groups ~'elements))

(defn delegate [attrs content] (fn-of (apply-of (first content))))

(defmacro def-base [expr base]
  (fn-of 
    `(let [t# ((~'types ~base) ~'value ~'types ~'attr-groups ~'elements)]
       [(and (first t# ) ~expr) (second t#)])))

(declare transform)
(declare transform-element)

(defn transform [the-map schema] 
  (cond
    (map? schema) (transform-element the-map schema)
    (string? schema) schema
    :else
    (map (partial transform the-map) schema)))

(defn transform-element [the-map element]
  (if-let [f (the-map (:tag element))]
    (f (:attrs element) (transform the-map (:content element)))
    (assoc element :content (transform the-map (:content element)))))

(defn parse-simple-type [attrs content]
  (condp = (.keySet attrs)
    #{:name} (with-meta {(:name attrs) (fn-of (apply-of (first content)))} {:kind :type})
    #{:name :type} (with-meta {(:name attrs) (fn-of `((~'types ~(:type attrs) ~@content) ~'value ~'types ~'attr-groups ~'elements))} {:kind :type})
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
  `(def-base 
     ~(if (enumeration? content)
       `(or ~@(transform restriction-map content))
       `(and ~@(transform restriction-map content)))
     ~(:base attrs)))

(defn throw-if-false [b] (if (first b) b (throw (IllegalArgumentException.))))

(defn add-try-catch [unions]
  (if (empty? (rest unions))
    `~(first unions)
    `(try (throw-if-false ~(first unions)) (catch Exception e# ~(add-try-catch (rest unions))))))
  
(defn element-of [attrs content]
  (apply-of `(~'types ~(:type attrs))))

(defn parse-element [attrs content]
 (with-meta 
   (condp = (.keySet attrs)
     #{:name :type}
     {(-> attrs :name keyword) (fn-of `(conj ~(element-of attrs content) ~(-> attrs :name keyword)))}
     #{:name} 
     {(-> attrs :name keyword) (fn-of `(conj ~(apply-of (first content)) ~(-> attrs :name keyword)))}
     #{:ref} content)
   {:kind :element}))

(defn member-types-of [member-types]
  (map (fn [m] (apply-of `(~'types ~m))) (.split member-types " ")))

(defn anom-types-of [types]
  (map apply-of types))

(defn parse-union 
  [attrs content]
    (fn-of 
      `~(add-try-catch 
          (concat
            (member-types-of (if-let [mt (:memberTypes attrs)] mt ""))
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
      `(let [~'types (merge ~(apply merge types) ~'types)
             elems# ~(apply merge elements)]
         ((elems# (:tag ~'value)) (content-of ~'value) ~'types ~'attr-groups ~'elements)))))



(defn parse-sequence [attrs content]
  (fn-of 
    `(let [~'elem-map ~(apply merge content)]
       (if ~'value
         (if
           (= (keys ~'elem-map) (map :tag ~'value)) ;TODO order!
           [true (map (fn [v#] ((~'elem-map (:tag v#)) (content-of v#) ~'types ~'attr-groups ~'elements)) ~'value)]
           [false []])
         [:sequence (keys ~'elem-map)]))))



(defn parse-choice [attrs content]
  (fn-of 
    `(let [~'elem-map ~(apply merge content)]
       (if ~'value
         (if
           (and (= (count ~'value) 1)
                (contains? (.keySet ~'elem-map) (-> ~'value first :tag)))
           [true ((~'elem-map (-> ~'value first :tag)) (content-of (first ~'value)) ~'types ~'attr-groups ~'elements)]
           [false []])
         [:choice (keys ~'elem-map)]))))

(defn parse-all [attrs content]
  (fn-of 
    `(let [~'elem-map ~(apply merge content)]
       (if ~'value
        (if
          (and 
            (= (count ~'elem-map) (count ~'value))
            (= (.keySet ~'elem-map) (set (map :tag ~'value))))
          [true (map (fn [v#] ((~'elem-map (:tag v#)) (content-of v#) ~'types ~'attr-groups ~'elements)) ~'value)]
          [false []])
        [:all (keys ~'elem-map)]))))

(defn remove-values-not-in-coll [values in-set]
  (let [in-set (set in-set)]
    (filter #(contains? in-set (:tag %)) values)))

(defn ext-and [base ext]
  [(and (first base) (first ext)) (concat (second base) (second ext))])

(defn parse-extension [attrs content]
  (fn-of 
    `(let [base-fn# (~'types ~(-> attrs :base)) 
           base# (base-fn# nil ~'types ~'attr-groups ~'elements)
           ext-fn# ~(first content)
           ext# (ext-fn# nil ~'types ~'attr-groups ~'elements)
           ]
       (if 
         ~'value
         (if (= (first base#) (first ext#))
           (ext-and
             (ext-fn# (remove-values-not-in-coll ~'value (second ext#)) ~'types ~'attr-groups ~'elements)
             (base-fn# (remove-values-not-in-coll ~'value (second base#)) ~'types ~'attr-groups ~'elements))
           [false []])
         [(first base#) (concat (second base#) (second ext#))])
         )))
  
(defn parse-attribute [attrs _]
    (if (contains? (.keySet attrs) :ref)
      :ref
      (let [n (-> attrs :name keyword)]
        (with-meta {n (fn-of `(conj ~(apply-of `(~'types ~(:type attrs))) ~n))} {:kind :attribute}) 
      )))



(def parse-map 
  {:simpleType parse-simple-type
   :attribute parse-attribute
   :restriction parse-restriction
   :union parse-union,
   :sequence parse-sequence
   :choice parse-choice
   :schema parse-schema
   :element parse-element
   :complexType parse-simple-type
   :complexContent delegate
   :extension parse-extension
   :all parse-all})

(def numeric? (fn [value _ _ _] [(number? value) value]))

(def predef-types
  {
   "string" (fn [value _ _ _] [(string? value) value])
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

(defprotocol IXmlParser
  (parse-xml [o]))

(extend-protocol IXmlParser
  String
  (parse-xml [s] (parse-str s))
  clojure.lang.PersistentVector
  (parse-xml [v] (sexp-as-element v))
  clojure.data.xml.Element
  (parse-xml [e] e) 
  )


(defn validation-expr-of [element]
  (transform parse-map (parse-xml element)))

(defn validation-fn-of [element]
  (eval (validation-expr-of element)))

(defn map-of [value]
  (if
    (coll? value)
    (if (instance? Boolean (first value))
      (let [tag (nth value 2)]
        (with-meta (cons tag (map-of (second value))) {tag (first value)}))
      (map map-of value))
    [value]))



(defn decode [schema value]
  (let [schema (validation-fn-of schema)]
    (-> value parse-xml (schema predef-types {} {}) map-of)))


(defn valid? [value]
  (letfn [(valid?-fn [value] 
            (let [n (rest value)
                  m (vals (meta value))]
                (concat m (map valid?-fn (filter coll? n)))))]
     (every? identity (flatten (valid?-fn value))))
  )

