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
	  (is (= true (f "small" predef-env)))
	  (is (= true (f "medium" predef-env)))
	  (is (= true (f "large" predef-env)))
	  (is (= false (f "asdf" predef-env)))
  ))

(deftest test-range-restriction
  (let [f (validation-fn-of "<restriction base=\"integer\">
		        <minInclusive value=\"36\"/>
		        <maxInclusive value=\"42\"/>
		      </restriction>")]
	  (is (= true (f 36 predef-env)))
	  (is (= true (f 42 predef-env)))
	  (is (= false (f 43 predef-env)))
  ))


(deftest test-anon-simple-type
  (let [f (validation-fn-of 
            "<simpleType>
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>")]
    
    (is (= true (f 36 predef-env)))
    (is (= false (f 43 predef-env)))))


(deftest test-simple-type-def
  (let [e (validation-expr-of 
            "<simpleType name=\"mytype\">
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>")]
    (is (= :type (-> e meta :kind)))
    (is (= false ((eval (first (vals e))) 0 predef-env)))
    (is (= true ((eval (first (vals e))) 36 predef-env)))
    ))

(deftest test-simple-type-reffing-predef
  (let [e (validation-expr-of 
            "<simpleType name=\"aname\" type=\"byte\">
             </simpleType>")]
    (is (= :type (-> e meta :kind)))
    (is (= true ((eval (first (vals e))) 0 predef-env)))
    (is (= true ((eval (first (vals e))) 127 predef-env)))
    (is (= false ((eval (first (vals e))) 128 predef-env)))
    
    ))

(deftest test-element
  (let [text "<element name=\"abyte\" type=\"byte\"/>"
        f (validation-fn-of text)]
    (is (= :element (-> (validation-expr-of text) meta :kind)))
    (is (= true ((f :abyte) 0 predef-env)))
    (is (= true ((f :abyte) 127 predef-env)))
    (is (= false ((f :abyte) 128 predef-env)))
  ))




(deftest test-schema-with-predefs
  (let [f (validation-fn-of 
            "<schema>
              <element name=\"abyte\" type=\"byte\"/>
              <element name=\"anint\" type=\"integer\"/>
             </schema>")]
    (is (= true (f (parse-str "<anint>0</anint>") predef-env)))
    (is (= true (f (parse-str "<abyte>0</abyte>") predef-env)))
    (is (= false (f (parse-str "<abyte>128</abyte>") predef-env)))
  ))

(deftest test-schema-with-simple-type
  (let [f (validation-fn-of 
            "<schema>
              <simpleType name=\"mytype\">
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>  
              <element name=\"abyte\" type=\"byte\"/>
              <element name=\"anint\" type=\"integer\"/>
              <element name=\"my\" type=\"mytype\"/>
             </schema>")]
    (is (= true (f (parse-str "<anint>0</anint>") predef-env)))
    (is (= true (f (parse-str "<abyte>0</abyte>") predef-env)))
    (is (= false (f (parse-str "<abyte>128</abyte>") predef-env)))
    (is (= true (f (parse-str "<my>36</my>") predef-env)))
    (is (= false (f (parse-str "<my>35</my>") predef-env)))
  ))


(deftest test-union
  (let [f (validation-fn-of 
            "<schema>
             <simpleType name=\"mytype\">
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>
             <simpleType name=\"myunion\">
                <union memberTypes=\"mytype\">  
                   <simpleType> 
                     <restriction base=\"string\">
                       <enumeration value=\"small\"/> 
                       <enumeration value=\"medium\"/> 
                       <enumeration value=\"large\"/> 
                     </restriction> 
                   </simpleType>
                </union>  
             </simpleType>
           <element name=\"theunion\" type=\"myunion\"/>
           </schema>")]
    (is (= false (f (parse-str "<theunion>35</theunion>") predef-env)))
    (is (= true (f (parse-str "<theunion>36</theunion>") predef-env)))
    (is (= true (f (parse-str "<theunion>small</theunion>") predef-env)))
    ))


(deftest test-sequence 
  (let [f (validation-fn-of 
          "<sequence>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </sequence>")
        v (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>"))]
    (is (= true (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq2>2</seq2><seq1>1</seq1></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-env)))
    ))
(deftest test-all 
  (let [f (validation-fn-of 
          "<all>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </all>")]
    (is (= true (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>")) predef-env)))
    (is (= true (f (:content (parse-str "<udr><seq2>2</seq2><seq1>1</seq1></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-env)))
    ))
(deftest test-choice 
  (let [f (validation-fn-of 
          "<choice>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </choice>")]
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq2>2</seq2><seq1>1</seq1></udr>")) predef-env)))
    (is (= true (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-env)))
    (is (= true (f (:content (parse-str "<udr><seq2>1</seq2></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-env)))
    (is (= false (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-env)))
    ))


(deftest test-complex-type 
  (let [f (validation-fn-of 
    "<schema>
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
		    <complexType name=\"cp\">
		      <sequence>
		        <element name=\"uniontest\" type=\"theunion\" maxOccurs=\"unbounded\"/>
		      </sequence>
		    </complexType>
		<element name=\"udr\" type=\"cp\">
		  </element>
    </schema>")]))
    