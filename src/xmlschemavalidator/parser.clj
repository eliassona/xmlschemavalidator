(ns xmlschemavalidator.parser
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta]
            [xmlschemavalidator.core :refer [dbg fn-of parse-xml 
                                             apply-of predef-types 
                                             def-base
                                             throw-if-false
                                             type? element?]]
            [clojure.data.xml :refer [parse-str sexp-as-element]]))

(defn simple-type? [value]
  (and (= (count value) 1) (not (map? (first value)))))

(defn special-read-string [s]
  (try 
    (let [v (read-string s)]
      (if (number? v)
        v
        s))
    (catch Exception e
      s))
    )

(defn content-of [value]
  (let [content (:content value)]
    (if (simple-type? content)
      (-> content first special-read-string)
      (with-meta content (:attrs value)))))

(defn attrs-of [attrs content]
  (if (empty? attrs)
    content
    (cons attrs content)))

(def min-max-occurs-attrs {:maxOccurs "1", :minOccurs "1"})
(def default-attrs {:sequence min-max-occurs-attrs
                    :all min-max-occurs-attrs
                    :choice min-max-occurs-attrs})
(defn assoc-attrs [tag attrs]
  (merge (default-attrs tag) attrs))

(defn element->hiccup [node]
  (cond
    (map? node)
    (let [tag (:tag node)]
      (cons tag (attrs-of (assoc-attrs tag (:attrs node)) (element->hiccup (:content node)))))
    (coll? node)
    (map element->hiccup node)
    :else
    node))

(def parser 
  (insta/parser "
                 SCHEMA = OPEN-PAREN <':schema'> SPACE TYPES CLOSE-PAREN
                 SCHEMA-BODY = ((INCLUDE | IMPORT | REDEFINE | ANNOTATION)* (((SIMPLETYPE | COMPLEXTYPE | GROUP | ATTRIBUTEGROUP) | ELEMENT | ATTRIBUTE | NOTATION) ANNOTATION*)*)
                 INCLUDE = OPEN-PAREN ':include' CLOSE-PAREN
                 IMPORT = OPEN-PAREN ':import' CLOSE-PAREN
                 REDEFINE = OPEN-PAREN ':redefine' CLOSE-PAREN
                 NOTATION = OPEN-PAREN ':notation' SPACE OPEN-BRACKET NAME-ATTR <','> SPACE ':public' SPACE STRING CLOSE-BRACKET CLOSE-PAREN
                 TYPES = ((TYPE OPTIONAL-SPACE)* | TYPE)
                 <TYPE> = (SIMPLETYPE | COMPLEXTYPE | ELEMENT | ATTRIBUTEGROUP | ATTRIBUTE)
                 ELEMENT = OPEN-PAREN <':element'> SPACE (NAME-TYPE-ATTR | (NAME-ATTR SPACE TYPE) | REF-ATTR) CLOSE-PAREN
                 REF-ATTR = OPEN-BRACKET <':ref'> SPACE SYMBOL CLOSE-BRACKET
                 COMPLEXTYPE = OPEN-PAREN <':complexType'> SPACE ((NAME-ATTR SPACE COMPLEXTYPE-BODY) | COMPLEXTYPE-BODY) CLOSE-PAREN
                 <COMPLEXTYPE-BODY> = [ANNOTATION] (SIMPLECONTENT | CONTENT | [COLLECTION]) OPTIONAL-SPACE ATTRIBUTES
                 <COLLECTION> = SEQUENCE | ALL | GROUP | CHOICE
                 SIMPLECONTENT = OPEN-PAREN ':simpleContent' SPACE [ANNOTATION] (RESTRICTION | EXTENSION) CLOSE-PAREN
                 COMPLEXCONTENT = OPEN-PAREN ':complexContent' SPACE EXTENSION CLOSE-PAREN
                 CONTENT = SIMPLECONTENT | COMPLEXCONTENT
                 EXTENSION = OPEN-PAREN ':extension' SPACE BASE-ATTR SPACE ([ANNOTATION] ([COLLECTION] ATTRIBUTES)) CLOSE-PAREN
                 ATTRIBUTEGROUP = OPEN-PAREN <':attributeGroup'> SPACE ((NAME-ATTR SPACE ATTRIBUTEGROUP-BODY) | REF-ATTR) CLOSE-PAREN
                 ATTRIBUTEGROUP-BODY = [ANNOTATION] ATTRIBUTES
                 <ATTR-OR-ATTRGROUP> = ATTRIBUTE | ATTRIBUTEGROUP
                 ATTRIBUTES = ((ATTR-OR-ATTRGROUP OPTIONAL-SPACE)* | ATTR-OR-ATTRGROUP)
                 ATTRIBUTE = OPEN-PAREN <':attribute'> SPACE ATTRIBUTE-ATTRS CLOSE-PAREN
                 <ATTRIBUTE-ATTRS> = ATTRIBUTE-NAME-TYPE | ATTRIBUTE-NAME-ONLY | REF-ATTR
                 ATTRIBUTE-NAME-ONLY = NAME-ATTR SPACE SIMPLETYPE
                 ATTRIBUTE-NAME-TYPE = (OPEN-BRACKET <':name'> SPACE SYMBOL <','> SPACE <':type'> SPACE SYMBOL [<','> SPACE (':default' | ':fixed' | ':use') SPACE SYMBOL] CLOSE-BRACKET)
                 GROUP = OPEN-PAREN ':group' SPACE (NAME-ATTR SPACE GROUP-BODY) | REF-ATTR | GROUP-BODY CLOSE-PAREN
                 GROUP-BODY = [ANNOTATION] [SEQUENCE | ALL | CHOICE]
                 ALL = OPEN-PAREN <':all'> SPACE [MIN-MAX-OCCURS-ATTRS] ELEMENTS CLOSE-PAREN
                 CHOICE = OPEN-PAREN <':choice'> SPACE [MIN-MAX-OCCURS-ATTRS] ELEMENTS CLOSE-PAREN
                 SEQUENCE = OPEN-PAREN <':sequence'> SPACE [MIN-MAX-OCCURS-ATTRS] ELEMENTS CLOSE-PAREN
                 MAX-OCCURS-ATTR = ':maxOccurs' SPACE STRING
                 MIN-OCCURS-ATTR = ':minOccurs' SPACE STRING
                 ELEMENTS = ((ELEMENT OPTIONAL-SPACE)* | ELEMENT)
                 <SIMPLETYPES> = ((SIMPLETYPE OPTIONAL-SPACE)* | SIMPLETYPE)
                 SIMPLETYPE = (OPEN-PAREN <':simpleType'> SPACE (SIMPLETYPE-BODY | ((NAME-TYPE-ATTR | (NAME-ATTR SPACE SIMPLETYPE-BODY)))) CLOSE-PAREN)
                 REF-ATTR = OPEN-BRACKET ':ref' SPACE SYMBOL CLOSE-BRACKET
                 NAME-ATTR = OPEN-BRACKET <':name'> SPACE SYMBOL CLOSE-BRACKET
                 NAME-TYPE-ATTR = OPEN-BRACKET <':name'> SPACE SYMBOL <','> SPACE <':type'> SPACE SYMBOL CLOSE-BRACKET
                 MIN-MAX-OCCURS-ATTRS = OPEN-BRACKET ':maxOccurs' SPACE STRING <','> SPACE ':minOccurs' SPACE STRING CLOSE-BRACKET OPTIONAL-SPACE
                 <SIMPLETYPE-BODY> = [ANNOTATION] (LIST | UNION | RESTRICTION)
                 LIST = OPEN-PAREN <':list'> SPACE OPEN-BRACKET ':itemType' SPACE SYMBOL CLOSE-BRACKET CLOSE-PAREN
                 UNION = OPEN-PAREN <':union'> SPACE [OPEN-BRACKET <':memberTypes'> OPTIONAL-SPACE MEMBERTYPES CLOSE-BRACKET] OPTIONAL-SPACE SIMPLETYPES CLOSE-PAREN
                 MEMBERTYPES = <'\"'> (NAME SPACE)* NAME <'\"'> 
                 ANNOTATION = OPEN-PAREN CLOSE-PAREN
                 RESTRICTION = OPEN-PAREN <':restriction'> SPACE BASE-ATTR SPACE RESTRICTION_BODIES CLOSE-PAREN
                 BASE-ATTR = OPEN-BRACKET <':base'> SPACE SYMBOL CLOSE-BRACKET
                 RESTRICTION_BODIES = ((RESTRICTION-BODY SPACE)* RESTRICTION-BODY)
                 RESTRICTION-BODY = OPEN-PAREN RESTRICTION-KWS SPACE OPEN-BRACKET <':value'> SPACE STRING CLOSE-BRACKET CLOSE-PAREN
                 RESTRICTION-KWS = ':enumeration' | ':minInclusive' | ':maxInclusive' | ':minExclusive' | ':maxExclusive' | ':pattern' | ':length' | ':minLength' | ':maxLength'
                 <OPEN-PAREN> = <'('>
                 <CLOSE-PAREN> = <')'>
                 <OPEN-BRACKET> = <'{'>
                 <CLOSE-BRACKET> = <'}'>
                 SYMBOL = <'\"'> #'[a-zA-Z_]'  #'\\w'* <'\"'>
                 NAME = #'[a-zA-Z_]'  #'\\w'*
                 STRING = <'\\\"'> #'([^\"\\\\]|\\\\.)*' <'\\\"'>
                 <SPACE> = <#'[ \t\n,]+'>
                 <OPTIONAL-SPACE> = <#'[ \t\n]*'>

"))

(defprotocol IXmlToHiccupParser
  (xml->hiccup [o]))

(extend-protocol IXmlToHiccupParser
  String
  (xml->hiccup [xml-syntax] (xml->hiccup (parse-str xml-syntax)))
  clojure.lang.PersistentVector
  (xml->hiccup [sexp-syntax] (xml->hiccup (sexp-as-element sexp-syntax)))
  clojure.data.xml.Element
  (xml->hiccup [xml-syntax] (-> xml-syntax element->hiccup str)))

(defn math-expr-of [op value] (with-meta (fn-of `(~op ~'value ~value)) {:kind :range}))
(defn length-expr-of [op value] (with-meta (fn-of `(~op (count (str ~'value)) ~value)) {:kind :range}))

(defn restriction->clj [tag value]
    (condp = tag
     :enumeration
     (with-meta (fn-of `(= ~value ~'value)) {:kind :enumeration})
     :minInclusive
     (math-expr-of `>= value)
     :maxInclusive
     (math-expr-of `<= value)
     :minExclusive
     (math-expr-of `> value)
     :maxExclusive
     (math-expr-of `< value)
     :length 
     (length-expr-of '= value)
     :minLength 
     (length-expr-of '> value)
     :maxLength 
     (length-expr-of '> value)
     :pattern
     (with-meta (fn-of `(.matches ~'value ~(format "%s" value))) {:kind :pattern})
     ))

(defn enumeration? [args] (every? #(= (-> % meta :kind) :enumeration) args))

(defn restrictions->clj [& args]
  (fn-of
    `[(~(if (enumeration? args) `or `and) ~@(map apply-of args)) ~'value]))

(defn simple-type->clj
  ([name the-fn]
    (with-meta {name the-fn} {:kind :type}))
  ([m] 
    (let [name (-> m meta :name)]
      (vary-meta m assoc :kind :type))))

(defn add-try-catch [unions]
  (if (empty? (rest unions))
    (apply-of `~(first unions))
    `(try (throw-if-false ~(apply-of (first unions))) (catch Exception e# ~(add-try-catch (rest unions))))))

(defn member-types->clj [& args]
  (fn-of (add-try-catch (map (fn [a] (fn-of (apply-of `(~'types ~a)))) args))))

(defn union->clj [& fns] (fn-of (add-try-catch fns)))

(defmacro def-attr [name expr]
  `(with-meta {~name (fn-of ~expr)} {:name ~name}))

(defn name-type->clj [name type] (def-attr (keyword name) (apply-of `(~'types ~type))))

(defn name->clj [name]
  (let [name name]
    name))
  
(defn element->clj 
  ([m]
    (if (and (vector? m) (= (first m) :ref))
      (let [name (-> m second keyword)]
        (with-meta {name (fn-of `((~'elements ~name) (-> ~'value :content) ~'types ~'attr-groups ~'elements))} (assoc (meta m) :kind :element)))
      (let [name (-> m meta :name)]
        (with-meta {name (fn-of `(conj ~(apply-of (m name)) ~name))} (assoc (meta m) :kind :element)))))
  ([name the-fn]
    (let [name (keyword name)]
      (with-meta {name (fn-of `(conj ~(apply-of the-fn) ~name))} {:kind :element}))))

(defn types->clj [& maps]
  (let [types (filter type? maps)
        elements (filter element? maps)]
    (fn-of 
      `(let [~'types (merge ~(apply merge types) ~'types)
             ~'elements (merge ~(apply merge elements) ~'elements)]
         ((~'elements (:tag ~'value)) (content-of ~'value) ~'types ~'attr-groups ~'elements)))
    ))
  
(defn elements->clj [& args] (apply merge args))

(defn container->clj [m cond-fn]
  (fn-of 
     `(let [~'elements (merge ~m ~'elements)] 
          (conj 
             (map 
               #(if-let 
                  [e# (~'elements (:tag %))] 
                  (e# (content-of %) ~'types ~'attr-groups ~'elements)
                  [false :undefined (:tag %)]
                  ) ~'value) 
             ~(cond-fn m 'value)))))

(defn sequence-cond->clj [m value] `(= (keys ~m) (map :tag ~value)))

(defmacro def-container [name]
  `(defn ~(symbol (format "%s->clj" name)) 
     ([m#] (container->clj m# ~(symbol (format "%s-cond->clj" name))))
     ([attrs# m#] (container->clj m# ~(symbol (format "%s-cond->clj" name))))))

(def-container sequence)

(defn all-cond->clj [m value] `(= (.keySet ~m) (set (map :tag ~value))))

(def-container all)

(defn choice-cond->clj [_ value] `(= (count ~value) 1))

(def-container choice)


(defn attr->clj [attrs]
  (fn-of 
     `[true 
       #{ 
          (conj
            (map 
             (fn [e#] (conj 
                        ((e# ~attrs) 
                          (-> ~'value meta e# special-read-string) ~'types ~'attr-groups ~'elements) e#)) 
             (keys ~attrs)) true)}]))

(defn coll->clj [coll attrs]
  (fn-of
    `(let [coll-res# (~coll ~'value ~'types ~'attr-groups ~'elements)]
       [true (second ~(apply-of (attr->clj attrs))) coll-res#])))

(defprotocol IComplexType
  (complex-type->clj [attrs] [o1 o2] [o1 o2 o3])
  )

(extend-protocol IComplexType
  String
  (complex-type->clj 
    ([name attrs]
      (with-meta {name (attr->clj attrs)} {:kind :type}))
    ([name coll attrs]
      (with-meta {name (coll->clj coll attrs)} {:kind :type})))
  clojure.lang.Cons
  (complex-type->clj 
    ([coll attrs] (coll->clj coll attrs)))
  java.util.Map
  (complex-type->clj 
    ([attrs]
      (attr->clj attrs))))


(defn attr-name-type->clj 
  ([name type]
    (name-type->clj name type))
  ([name type kind kind-value]
    (let [name (keyword name)]
      (condp = (read-string kind)
        :default
        (def-attr name 
          `(if (nil? ~'value)
             ((~'types ~type) ~kind-value ~'types ~'attr-groups ~'elements)
             ~(apply-of `(~'types ~type))))
        :fixed
       (def-attr name
         `(if (not= ~'value ~kind-value)
            [false ~'value]
            ~(apply-of `(~'types ~type)))) 
        :use
        (def-attr name
             `(if (nil? ~'value)
                [false :undefined]
                ~(apply-of `(~'types ~type))))))))
  
    

(def ast->clj-map
  {:SYMBOL (fn [& args] (apply str args))
   :NAME (fn [& args] (apply str args))
   :NAME-TYPE-ATTR name-type->clj
   :NAME-ATTR name->clj
   :STRING #(-> % special-read-string)
   :RESTRICTION-KWS identity
   :RESTRICTION-BODY (fn [tag value] (restriction->clj (read-string tag) value))
   :RESTRICTION_BODIES restrictions->clj 
   :BASE-ATTR (fn [base] (fn-of (apply-of `(~'types ~base))))
   :RESTRICTION (fn [base restriction] (fn-of `(and ~(apply-of base) ~(apply-of restriction))))
   :SIMPLETYPE simple-type->clj
   :MEMBERTYPES member-types->clj
   :ELEMENT element->clj
   :UNION union->clj
   :TYPES types->clj
   :SCHEMA identity
   :ELEMENTS elements->clj
   :SEQUENCE sequence->clj
   :ATTRIBUTE-NAME-TYPE attr-name-type->clj
   :ATTRIBUTE identity
   :CHOICE choice->clj
   :ALL all->clj
   :COMPLEXTYPE complex-type->clj
   :ATTRIBUTES (fn [& args] (apply merge args))
   :REF-ATTR (fn [_ name] [:ref name])
   }
  )

(defn ast->clj [ast] (insta/transform ast->clj-map ast))


(defn validation-expr-of 
  ([schema start]
  (-> schema xml->hiccup (parser :start start) ast->clj))
  ([schema]
    (validation-expr-of schema :SCHEMA)))

(defn validation-fn-of 
  ([schema start]
    (eval (validation-expr-of schema start)))
  ([schema]
    (eval (validation-expr-of schema))))



(defprotocol IHiccupWithMeta
  (to-hiccup-meta [o]))

(defn to-map-with-meta [acc e]
  (let [[k v] e] 
    (vary-meta (assoc acc k v) merge (meta e))))

(extend-protocol IHiccupWithMeta
  nil
  (to-hiccup-meta [_] nil)
  java.util.List
  (to-hiccup-meta [l]
    (if (and (= (count l) 4) (set? (second l)))
      (let [[status attrs content tag] l
            a (to-hiccup-meta attrs)]
        (if (empty? a)
          (with-meta [tag (to-hiccup-meta content) ] {tag status})
          (with-meta [tag a (to-hiccup-meta content) ] {tag status})))
      (let [[status content tag] l] 
        (if (nil? tag)
          (to-hiccup-meta content)
          (with-meta [tag (to-hiccup-meta content) ] {tag status})))))
  java.util.Set
  (to-hiccup-meta [s]
    (reduce to-map-with-meta {} (map (fn [[status value tag]] (with-meta [tag value] {tag status})) (-> s first rest))))
  Object
  (to-hiccup-meta [o] o))

(defn decode [schema value]
  (let [schema (validation-fn-of schema)]
    (-> value parse-xml (schema predef-types {} {}) to-hiccup-meta)))

(defn valid? [value]
  (letfn [(valid?-fn [value] 
            (let [n (rest value)
                  m (vals (meta value))]
                (concat m (map valid?-fn (filter coll? n)))))]
     (every? identity (flatten (valid?-fn value)))))

(defprotocol IWithStatus
  (with-status [v status] [v]))

(extend-protocol IWithStatus
  clojure.lang.PersistentVector
  (with-status 
    ([v _] (mapv #(with-status % ((meta v) (first v))) v))
    ([v] (with-status v nil)))
  clojure.lang.Keyword
  (with-status [v _] v)
  String
  (with-status [v status] {:value v, :status status})
  java.util.Map
  (with-status [v _] (into {} (map (fn [e] [(key e) (with-status (val e) ((meta v) (key e)))]) v)))
  Number
  (with-status [v status] {:value v, :status status})
  )

