(ns xmlschemavalidator.parser
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta]
            [xmlschemavalidator.core :refer [fn-of apply-of predef-types]]
            [clojure.data.xml :refer [parse-str]]))


(defn attrs-of [attrs content]
  (if (empty? attrs)
    content
    (cons attrs content)))

(defn element->hiccup [node]
  (cond
    (map? node) 
    (cons (:tag node) (attrs-of (:attrs node) (element->hiccup (:content node))))
    (coll? node)
    (map element->hiccup node)
    :else
    node))




(def parser 
  (insta/parser "
                 SCHEMA = OPEN-PAREN ':schema' SPACE TYPES CLOSE-PAREN
                 SCHEMA-BODY = ((INCLUDE | IMPORT | REDEFINE | ANNOTATION)* (((SIMPLETYPE | COMPLEXTYPE | GROUP | ATTRIBUTEGROUP) | ELEMENT | ATTRIBUTE | NOTATION) ANNOTATION*)*)
                 INCLUDE = OPEN-PAREN ':include' CLOSE-PAREN
                 IMPORT = OPEN-PAREN ':import' CLOSE-PAREN
                 REDEFINE = OPEN-PAREN ':redefine' CLOSE-PAREN
                 NOTATION = OPEN-PAREN ':notation' SPACE OPEN-BRACKET NAME-ATTR <','> SPACE ':public' SPACE STRING CLOSE-BRACKET CLOSE-PAREN
                 TYPES = ((TYPE OPTIONAL-SPACE)* | TYPE)
                 TYPE = (SIMPLETYPE | COMPLEXTYPE | ELEMENT | ATTRIBUTEGROUP | ATTRIBUTE)
                 ELEMENT = OPEN-PAREN ':element' SPACE (NAME-TYPE-ATTR | (NAME-ATTR SPACE TYPE)) CLOSE-PAREN
                 COMPLEXTYPE = OPEN-PAREN ':complexType' SPACE ((NAME-ATTR SPACE COMPLEXTYPE-BODY) | COMPLEXTYPE-BODY) CLOSE-PAREN
                 COMPLEXTYPE-BODY = [ANNOTATION] (SIMPLECONTENT | CONTENT | [COLLECTION]) ATTRIBUTES
                 COLLECTION = SEQUENCE | ALL | GROUP | CHOICE
                 SIMPLECONTENT = OPEN-PAREN ':simpleContent' SPACE [ANNOTATION] (RESTRICTION | EXTENSION) CLOSE-PAREN
                 COMPLEXCONTENT = OPEN-PAREN ':complexContent' SPACE EXTENSION CLOSE-PAREN
                 CONTENT = SIMPLECONTENT | COMPLEXCONTENT
                 EXTENSION = OPEN-PAREN ':extension' SPACE BASE-ATTR SPACE ([ANNOTATION] ([COLLECTION] ATTRIBUTES)) CLOSE-PAREN
                 ATTRIBUTEGROUP = OPEN-PAREN ':attributeGroup' SPACE ((NAME-ATTR SPACE ATTRIBUTEGROUP-BODY) | REF-ATTR) CLOSE-PAREN
                 ATTRIBUTEGROUP-BODY = [ANNOTATION] ATTRIBUTES
                 ATTR-OR-ATTRGROUP = ATTRIBUTE | ATTRIBUTEGROUP
                 ATTRIBUTES = ((ATTR-OR-ATTRGROUP OPTIONAL-SPACE)* | ATTR-OR-ATTRGROUP)
                 ATTRIBUTE = OPEN-PAREN ':attribute' SPACE ATTRIBUTE-ATTRS CLOSE-PAREN
                 ATTRIBUTE-ATTRS = ATTRIBUTE-NAME-TYPE | ATTRIBUTE-NAME-ONLY | REF-ATTR
                 ATTRIBUTE-NAME-ONLY = NAME-ATTR SPACE SIMPLETYPE
                 ATTRIBUTE-NAME-TYPE = (OPEN-BRACKET ':name' SPACE SYMBOL <','> SPACE ':type' SPACE SYMBOL [<','> SPACE (':default' | ':fixed' | ':use') SPACE SYMBOL] CLOSE-BRACKET)
                 GROUP = OPEN-PAREN ':group' SPACE (NAME-ATTR SPACE GROUP-BODY) | REF-ATTR | GROUP-BODY CLOSE-PAREN
                 GROUP-BODY = [ANNOTATION] [SEQUENCE | ALL | CHOICE]
                 ALL = OPEN-PAREN ':all' SPACE ELEMENTS CLOSE-PAREN
                 CHOICE = OPEN-PAREN ':choice' SPACE ELEMENTS CLOSE-PAREN
                 SEQUENCE = OPEN-PAREN ':sequence' SPACE ELEMENTS CLOSE-PAREN
                 ELEMENTS = ((ELEMENT OPTIONAL-SPACE)* | ELEMENT)
                 SIMPLETYPES = ((SIMPLETYPE OPTIONAL-SPACE)* | SIMPLETYPE)
                 SIMPLETYPE = (OPEN-PAREN ':simpleType' SPACE (SIMPLETYPE-BODY | ((NAME-TYPE-ATTR | (NAME-ATTR SPACE SIMPLETYPE-BODY)))) CLOSE-PAREN)
                 REF-ATTR = OPEN-BRACKET ':ref' SPACE SYMBOL CLOSE-BRACKET
                 NAME-ATTR = OPEN-BRACKET ':name' SPACE SYMBOL CLOSE-BRACKET
                 NAME-TYPE-ATTR = OPEN-BRACKET ':name' SPACE SYMBOL <','> SPACE ':type' SPACE SYMBOL CLOSE-BRACKET
                 SIMPLETYPE-BODY = [ANNOTATION] (LIST | UNION | RESTRICTION)
                 LIST = OPEN-PAREN ':list' SPACE OPEN-BRACKET ':itemType' SPACE SYMBOL CLOSE-BRACKET CLOSE-PAREN
                 UNION = OPEN-PAREN ':union' [SPACE OPEN-BRACKET ':memberTypes' SPACE MEMBERTYPES CLOSE-BRACKET] SIMPLETYPES CLOSE-PAREN
                 MEMBERTYPES = <'\"'> (NAME SPACE)* NAME <'\"'> 
                 ANNOTATION = OPEN-PAREN CLOSE-PAREN
                 
                 RESTRICTION = OPEN-PAREN <':restriction'> SPACE BASE-ATTR SPACE RESTRICTION_BODIES CLOSE-PAREN
                 BASE-ATTR = OPEN-BRACKET <':base'> SPACE SYMBOL CLOSE-BRACKET
                 RESTRICTION_BODIES = ((RESTRICTION-BODY SPACE)* RESTRICTION-BODY)
                 RESTRICTION-BODY = OPEN-PAREN RESTRICTION-KWS SPACE OPEN-BRACKET <':value'> SPACE STRING CLOSE-BRACKET CLOSE-PAREN
                 RESTRICTION-KWS = ':enumeration' | ':minInclusive' | ':maxInclusive' | ':minExclusive' | ':maxExclusive' | ':pattern'
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


(defprotocol IParser
  (xml-parse [o]))

(extend-protocol IParser
  String
  (xml-parse [xml-syntax] (xml-parse (parse-str xml-syntax)))
  clojure.data.xml.Element
  (xml-parse [xml-syntax] (-> xml-syntax element->hiccup str)))


(defn restrction->clj [tag value]
    (condp = tag
     :enumeration
     (with-meta (fn-of `(= ~value ~'value)) {:kind :enumeration})
     ))

(defn enumeration? [args]
  (every? #(= (-> % meta :kind) :enumeration) args))

(defn restrictions->clj [& args]
  (fn-of
    `(~(if (enumeration? args) `or `and) ~@(map apply-of args))))

(def ast->clj-map
  {:SYMBOL (fn [& args] (apply str args))
   :NAME (fn [& args] (apply str args))
   :STRING identity
   :RESTRICTION-KWS identity
   :RESTRICTION-BODY (fn [tag value] (restrction->clj (read-string tag) value))
   :RESTRICTION_BODIES restrictions->clj 
   :BASE-ATTR (fn [base] (fn-of (apply-of `(~'types ~base))))
   :RESTRICTION (fn [base restriction] (fn-of `(and ~(apply-of base) ~(apply-of restriction))))
  ; :OPEN-BRACKET (fn [& args] (map read-string args))
   }
  )

(defn ast->clj [ast]
  (insta/transform ast->clj-map ast))

