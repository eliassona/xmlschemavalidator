(ns xmlschemavalidator.parser_test
  (:use [clojure.pprint])
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [clojure.data.xml :refer [parse-str parse sexp-as-element emit-str]]
            [xmlschemavalidator.core :refer [predef-types dbg apply-of]]
            [xmlschemavalidator.parser :refer :all]))


(defn assert-parsing [& texts]
  (doseq [[start t] texts]
    (let [res (-> t xml-parse (parser :start start))]
      (is (= false (insta/failure? res)) (insta/get-failure res)))))

(deftest test-parsing
  
	(assert-parsing
	  [:SCHEMA "<schema>
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
	         <element name=\"uniontest\" type=\"theunion\"/>
	       </sequence>
	     </complexType>
			<element name=\"udr\" type=\"cp\">
			  </element>
	    </schema>"
	  "]
		 [:ELEMENT <element name=\"note\">
		  <complexType>
		    <sequence>
				  <element name=\"to\" type=\"string\"/>
				  <element name=\"from\" type=\"string\"/>
				  <element name=\"heading\" type=\"string\"/>
				  <element name=\"body\" type=\"string\"/>
		    </sequence>
		  </complexType>
		  </element>
		"]
		
			[:COMPLEXTYPE "<complexType name=\"fullpersoninfo\">
			  <complexContent>
			    <extension base=\"personinfo\">
			      <sequence>
			        <element name=\"address\" type=\"string\"/>
			        <element name=\"city\" type=\"string\"/>
			        <element name=\"country\" type=\"string\"/>
			      </sequence>
			    </extension>
			  </complexContent>
			</complexType>"]
	
	  [:ELEMENT "<element name=\"shoesize\">
		  <complexType>
		    <simpleContent>
		      <extension base=\"integer\">
		        <attribute name=\"country\" type=\"string\" />
		      </extension>
		    </simpleContent>
		  </complexType>
		</element>"]

  [:SCHEMA 
   "
   <schema> 
    <attributeGroup name=\"personattr\">
      <attribute name=\"attr1\" type=\"string\"/>
      <attribute name=\"attr2\" type=\"integer\"/>
    </attributeGroup>
    <complexType name=\"person\">
      <attributeGroup ref=\"personattr\"/>
    </complexType>
   </schema>"
   ]
   
   [:SCHEMA 
 "<schema>
   <attribute name=\"code\">
   <simpleType>
    <restriction base=\"string\">
      <pattern value=\"[A-Z][A-Z]\"/>
    </restriction>
   </simpleType>
   </attribute>

   <complexType name=\"someComplexType\">
     <attribute ref=\"code\"/>
   </complexType>
 </schema>"]
   
   #_[:SCHEMA "
    <schema>
			<group name=\"custGroup\">
			  <sequence>
			    <element name=\"customer\" type=\"string\"/>
			    <element name=\"orderdetails\" type=\"string\"/>
			    <element name=\"billto\" type=\"string\"/>
			    <element name=\"shipto\" type=\"string\"/>
			  </sequence>
			</group>
<!--			
			<element name=\"order\" type=\"ordertype\"/>
			<complexType name=\"ordertype\">
			  <group ref=\"custGroup\"/>
			  <attribute name=\"status\" type=\"string\"/>
			</complexType>
-->			
  </schema>"]
   [:UNION
     "<union>
            <simpleType>
                <restriction base=\"positiveInteger\">
                    <enumeration value=\"20\"/>
                    <enumeration value=\"30\"/>
                    <enumeration value=\"40\"/>
                </restriction>
            </simpleType>
            <simpleType>
                <restriction base=\"positiveInteger\">
                    <minInclusive value=\"2\"/>
                    <maxInclusive value=\"18\"/>
                </restriction>
            </simpleType>
            <simpleType>
                <restriction base=\"string\">
                    <enumeration value=\"small\"/>
                    <enumeration value=\"medium\"/>
                    <enumeration value=\"large\"/>
                </restriction>
            </simpleType>
            <simpleType>
                <restriction base=\"string\">
                    <enumeration value=\"S\"/>
                    <enumeration value=\"M\"/>
                    <enumeration value=\"L\"/>
                </restriction>
            </simpleType>
        </union>"]   
   
  ))


(deftest test-enum-restriction
  (let [f (validation-fn-of "<restriction base=\"string\">
		        <enumeration value=\"small\"/>
		        <enumeration value=\"medium\"/>
		        <enumeration value=\"large\"/>
		      </restriction>" :RESTRICTION)]
	  (is (= [true "small"] (f "small" predef-types {} {})))
	  (is (= [true "medium"] (f "medium" predef-types {} {})))
	  (is (= [true "large"] (f "large" predef-types {} {})))
	  (is (= [false "asdf"] (f "asdf" predef-types {} {})))
  ))

(deftest test-range-restriction
  (let [f (validation-fn-of "<restriction base=\"integer\">
		        <minInclusive value=\"36\"/>
		        <maxInclusive value=\"42\"/>
		      </restriction>" :RESTRICTION)]
	  (is (= [true 36] (f 36 predef-types {} {})))
	  (is (= [true 42] (f 42 predef-types {} {})))
	  (is (= [false 43] (f 43 predef-types {} {})))
  ))

(deftest test-anon-simple-type
  (let [f (validation-fn-of 
            "<simpleType>
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>" :SIMPLETYPE)]
    
    (is (= [true 36] (f 36 predef-types {} {})))
    (is (= [false 43] (f 43 predef-types {} {})))))

(deftest test-simple-type-def
  (let [e (validation-expr-of 
            "<simpleType name=\"mytype\">
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>" :SIMPLETYPE)]
    (is (= :type (-> e meta :kind)))
    (is (= [false 0] ((eval (first (vals e))) 0 predef-types {} {})))
    (is (= [true 36] ((eval (first (vals e))) 36 predef-types {} {})))
    ))

(deftest test-simple-type-reffing-predef
  (let [e (validation-expr-of 
            "<simpleType name=\"aname\" type=\"byte\">
             </simpleType>" :SIMPLETYPE)]
    (is (= :type (-> e meta :kind)))
    (is (= [true 0] ((eval (first (vals e))) 0 predef-types {} {})))
    (is (= [true 127] ((eval (first (vals e))) 127 predef-types {} {})))
    (is (= [false 128] ((eval (first (vals e))) 128 predef-types {} {})))
    
    ))
(deftest test-element
  (let [text "<element name=\"abyte\" type=\"byte\"/>"
        f (validation-fn-of text :ELEMENT)]
    (is (= :element (-> (validation-expr-of text :ELEMENT) meta :kind)))
    (is (= [true 0 :abyte] ((f :abyte) 0 predef-types {} {})))
    (is (= [true 127 :abyte] ((f :abyte) 127 predef-types {} {})))
    (is (= [false 128 :abyte] ((f :abyte) 128 predef-types {} {})))
  ))

(deftest test-schema-with-predefs
  (let [f (validation-fn-of 
              "<schema>
              <element name=\"abyte\" type=\"byte\"/>
              <element name=\"anint\" type=\"integer\"/>
             </schema>")]
      (is (= [true 0 :anint] (f (parse-str "<anint>0</anint>") predef-types {} {})))
      (is (= [true 0 :abyte] (f (parse-str "<abyte>0</abyte>") predef-types {} {})))
      (is (= [false 128 :abyte] (f (parse-str "<abyte>128</abyte>") predef-types {} {})))
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
    (is (= [true 0 :anint] (f (parse-str "<anint>0</anint>") predef-types {} {})))
    (is (= [true 0 :abyte] (f (parse-str "<abyte>0</abyte>") predef-types {} {})))
    (is (= [false 128 :abyte] (f (parse-str "<abyte>128</abyte>") predef-types {} {})))
    (is (= [true 36 :my] (f (parse-str "<my>36</my>") predef-types {} {})))
    (is (= [false 35 :my] (f (parse-str "<my>35</my>") predef-types {} {})))
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
    (is (= [false 35 :theunion] (f (parse-str "<theunion>35</theunion>") predef-types {} {})))
    (is (= [true 36 :theunion] (f (parse-str "<theunion>36</theunion>") predef-types {} {})))
    (is (= [true "small" :theunion] (f (parse-str "<theunion>small</theunion>") predef-types {} {})))
    (is (= [true "medium" :theunion] (f (parse-str "<theunion>medium</theunion>") predef-types {} {})))
    (is (= [false "asdf" :theunion] (f (parse-str "<theunion>asdf</theunion>") predef-types {} {})))
    ))


(deftest test-sequence 
  (let [f (validation-fn-of 
          "<sequence>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </sequence>" :SEQUENCE)]
    (is (= [true [[true 1 :seq1] [true "adsf" :seq2]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>adsf</seq2></udr>")) predef-types {} {})))
    (is (= [false [[true "adsf" :seq2] [true 1 :seq1]]] (f (:content (parse-str "<udr><seq2>2</seq2><seq1>asdf</seq1></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1]]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1] [false 2 :seq2] [false :undefined :seq3]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1] [false :undefined :seq3]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))

(deftest test-all 
  (let [f (validation-fn-of 
          "<all>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </all>" :ALL)]
    (is (= [true [[true 1 :seq1][true "asdf" :seq2]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>asdf</seq2></udr>")) predef-types {} {})))
    (is (= [true [[true "asdf" :seq2][true 10 :seq1]]] (f (:content (parse-str "<udr><seq2>asdf</seq2><seq1>10</seq1></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1]]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1] [false 2 :seq2] [false :undefined :seq3]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1] [false :undefined :seq3]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))
(deftest test-choice 
  (let [f (validation-fn-of 
          "<choice>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </choice>" :CHOICE)]
    (is (= [false [[true 1 :seq1] [false 2 :seq2]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>")) predef-types {} {})))
    (is (= [false [[false 2 :seq2] [true 1 :seq1]]] (f (:content (parse-str "<udr><seq2>2</seq2><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [true [[true 1 :seq1]]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [true [[true "asdf" :seq2]]] (f (:content (parse-str "<udr><seq2>asdf</seq2></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1] [false 2 :seq2] [false :undefined :seq3]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false [[true 1 :seq1] [false :undefined :seq3]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))


(deftest test-complex-type
  (let [e (validation-expr-of 
            "<complexType name=\"cp\">
<!--
              <sequence>
                <element name=\"seq\" type=\"string\"/>
              </sequence>
-->
              <attribute name=\"country\" type=\"string\"/>
              <attribute name=\"zip\" type=\"integer\"/>
             </complexType>" :COMPLEXTYPE)]
    (is (= :type (-> e meta :kind)))
;    (is (= [true 0] ((eval (first (vals e))) (parse-str "<udr country=\"usa\" zip=\"13672\"><seq>asdf</seq></udr>") predef-types {} {})))
    
    ))


