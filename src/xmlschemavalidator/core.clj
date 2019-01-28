(ns xmlschemavalidator.core
  (:use [clojure.pprint])
  (:require [clojure.data.xml :refer [parse-str parse]])
  (:import [java.io ByteArrayInputStream InputStream])
  )


(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))
(defprotocol ISchemaParser
  (parse-schema [schema]))

(defn type-of [type]
  (when (= (:tag type) :element)
    [(-> type :attrs :name keyword) type]))

(defn types-of [env]
  (into {} (map type-of env)))
 

(defn validate-schema [data node env]
  (dbg node)
    (if-let [type (dbg (-> node keys))]
      (dbg type)
      )
  )

(defprotocol IValidate 
  (validate [data node env]))
  
  
(extend-protocol IValidate
  java.util.Map
  (validate [data node env]
    (validate (:content data) node env))
  clojure.lang.LazySeq
  (validate [data node env]
    (validate (first data) node env))
  String
  (validate [data node env]
    (validate-schema (.trim data) node env))
  )

  (extend-protocol ISchemaParser
    String
    (parse-schema [schema] (parse-schema (ByteArrayInputStream. (.getBytes schema))))
    InputStream
    (parse-schema [schema] 
      (let [schema (parse schema)
            env (if (= (:tag schema) :schema) (:content schema) (throw (IllegalArgumentException. "Invalid schema")))]
        (fn [data]
          (let [data (parse-str data)]
            (validate data env env))))))
      




  (def schema 
    "<schema>
    <element name=\"sizebystring\">
	    <simpleType>
	      <restriction base=\"integer\">
	        <enumeration value=\"small\"/>
	        <enumeration value=\"medium\"/>
	        <enumeration value=\"large\"/>
	      </restriction>
	    </simpleType>
    </element>
  </schema>")

  (def data 
    "<sizebystring>
     small
   </sizebystring>")

  (comment
    ((parse-schema schema) data)
    )
  
