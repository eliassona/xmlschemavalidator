(ns xmlschemavalidator.core-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clojure.data.xml :refer [parse-str parse]]
            [xmlschemavalidator.lazymap :refer [lazy-map]]
            [xmlschemavalidator.core :refer :all]))



(def schema "<schema>
    <simpleType name=\"stringenum\">
      <restriction base=\"string\">
        <enumeration value=\"small\"/>
        <enumeration value=\"medium\"/>
        <enumeration value=\"large\"/>
      </restriction>
    </simpleType>
		<simpleType name =\"intrange\">
		      <restriction base=\"integer\">
		        <minInclusive value=\"36\"/>
		        <maxInclusive value=\"42\"/>
		      </restriction>
		    </simpleType>
		    <simpleType name =\"theunion\">
		    <union memberTypes=\"stringenum intrange\"/>
		    </simpleType>
		<element name=\"udr\">
		    <complexType>
		      <sequence>
		        <element name=\"uniontest\" type=\"theunion\" maxOccurs=\"unbounded\"/>
		      </sequence>
		    </complexType>
		  </element>
</schema>")

(def value 
  "<udr>
     <uniontest>36</uniontest>
     <uniontest>40</uniontest>
   </udr>")

(def schema-res
    (fn [data] {:udr {:uniontest (fn [data])}}))


(deftest test-enum-restriction
  (let [f (validation-fn-of "<restriction base=\"string\">
		        <enumeration value=\"small\"/>
		        <enumeration value=\"medium\"/>
		        <enumeration value=\"large\"/>
		      </restriction>")]
	  (is (= true (f "small" nil)))
	  (is (= true (f "medium" nil)))
	  (is (= true (f "large" nil)))
	  (is (= false (f "asdf" nil)))
  ))

(deftest test-range-restriction
  (let [f (validation-fn-of "<restriction base=\"integer\">
		        <minInclusive value=\"36\"/>
		        <maxInclusive value=\"42\"/>
		      </restriction>")]
	  (is (= true (f 36 nil)))
	  (is (= true (f 42 nil)))
	  (is (= false (f 43 nil)))
  ))


(deftest test-anon-simple-type
  (let [f (validation-fn-of 
            "<simpleType>
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>")]
    
    (is (= true (f [36] nil)))
    (is (= false (f [43] nil)))))

(deftest test-type-simple-type
  (let [e (validation-expr-of 
            "<simpleType name=\"aname\" type=\"byte\">
             </simpleType>")]
    (is (= :type (-> e meta :kind)))
    (is (= true ((eval (second e)) [0] predef-env)))
    (is (= true ((eval (second e)) [127] predef-env)))
    (is (= false ((eval (second e)) [128] predef-env)))
    
    ))

(deftest test-element
  (let [text "<element name=\"abyte\" type=\"byte\"/>"
        f (validation-fn-of text)]
    (is (= :element (-> (validation-expr-of text) meta :kind)))
    (is (= true ((f :abyte) 0 predef-env)))
    (is (= true ((f :abyte) 127 predef-env)))
    (is (= false ((f :abyte) 128 predef-env)))
  ))

(deftest test-schema
  #_(let [v (parse-str "<anint>0</anint>")
         f (validation-fn-of 
             "<schema>
              <element name=\"abyte\" type=\"byte\"/>
              <element name=\"anint\" type=\"integer\"/>
             </schema>")]
     (is (= true (f v predef-env)))
  ))
