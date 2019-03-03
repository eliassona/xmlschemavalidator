(ns xmlschemavalidator.core-test
  (:use clojure.pprint)
  (:require [clojure.test :refer :all]
            [clojure.data.xml :refer [parse-str parse sexp-as-element]]
            [xmlschemavalidator.lazymap :refer [lazy-map]]
            [xmlschemavalidator.core :refer :all]))

(deftest test-enum-restriction
  (let [f (validation-fn-of "<restriction base=\"string\">
		        <enumeration value=\"small\"/>
		        <enumeration value=\"medium\"/>
		        <enumeration value=\"large\"/>
		      </restriction>")]
	  (is (= [true "small"] (f "small" predef-types {} {})))
	  (is (= [true "medium"] (f "medium" predef-types {} {})))
	  (is (= [true "large"] (f "large" predef-types {} {})))
	  (is (= [false "asdf"] (f "asdf" predef-types {} {})))
  ))

(deftest test-range-restriction
  (let [f (validation-fn-of "<restriction base=\"integer\">
		        <minInclusive value=\"36\"/>
		        <maxInclusive value=\"42\"/>
		      </restriction>")]
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
             </simpleType>")]
    
    (is (= [true 36] (f 36 predef-types {} {})))
    (is (= [false 43] (f 43 predef-types {} {})))))


(deftest test-simple-type-def
  (let [e (validation-expr-of 
            "<simpleType name=\"mytype\">
              <restriction base=\"integer\">
		            <minInclusive value=\"36\"/>
		            <maxInclusive value=\"42\"/>
		          </restriction>
             </simpleType>")]
    (is (= :type (-> e meta :kind)))
    (is (= [false 0] ((eval (first (vals e))) 0 predef-types {} {})))
    (is (= [true 36] ((eval (first (vals e))) 36 predef-types {} {})))
    ))

(deftest test-simple-type-reffing-predef
  (let [e (validation-expr-of 
            "<simpleType name=\"aname\" type=\"byte\">
             </simpleType>")]
    (is (= :type (-> e meta :kind)))
    (is (= [true 0] ((eval (first (vals e))) 0 predef-types {} {})))
    (is (= [true 127] ((eval (first (vals e))) 127 predef-types {} {})))
    (is (= [false 128] ((eval (first (vals e))) 128 predef-types {} {})))
    
    ))

(deftest test-element
  (let [text "<element name=\"abyte\" type=\"byte\"/>"
        f (validation-fn-of text)]
    (is (= :element (-> (validation-expr-of text) meta :kind)))
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
    ))


(deftest test-sequence 
  (let [f (validation-fn-of 
          "<sequence>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </sequence>")]
    (is (= [true [[true 1 :seq1] [true "adsf" :seq2]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>adsf</seq2></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq2>2</seq2><seq1>asdf</seq1></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))
(deftest test-all 
  (let [f (validation-fn-of 
          "<all>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </all>")]
    (is (= [true [[true 1 :seq1][true "asdf" :seq2]]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>asdf</seq2></udr>")) predef-types {} {})))
    (is (= [true [[true "asdf" :seq2][true 10 :seq1]]] (f (:content (parse-str "<udr><seq2>asdf</seq2><seq1>10</seq1></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))
(deftest test-choice 
  (let [f (validation-fn-of 
          "<choice>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </choice>")]
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq2>2</seq2><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [true [true 1 :seq1]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [true [true "asdf" :seq2]] (f (:content (parse-str "<udr><seq2>asdf</seq2></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false []] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
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
         <element name=\"uniontest\" type=\"theunion\"/>
       </sequence>
     </complexType>
		<element name=\"udr\" type=\"cp\">
		  </element>
    </schema>")]
    (is (= [true [[false 0 :uniontest]] :udr] (f (parse-str "<udr><uniontest>0</uniontest></udr>") predef-types {} {})))
    (is (= [true [[true 36 :uniontest]] :udr] (f (parse-str "<udr><uniontest>36</uniontest></udr>") predef-types {} {})))
    (is (= [true [[true "small" :uniontest]] :udr] (f (parse-str "<udr><uniontest>small</uniontest></udr>") predef-types {} {})))
    (is (= [true [[false "randomstring" :uniontest]] :udr] (f (parse-str "<udr><uniontest>randomstring</uniontest></udr>") predef-types {} {})))
    
    ))
    

(deftest test-decode
  (let [schema "<schema>
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
    value (decode schema [:udr [:uniontest 0]])]
    (is (= [:udr [:uniontest 0]] value))
    (is (= {:udr true} (meta value)))
    (is (= false (valid? value)))
    (is (= {:uniontest false} (-> value second meta)))
    (is (= {:uniontest true} (-> (decode schema [:udr [:uniontest 36]]) second meta)))
    ))

(deftest test-ref
           (validation-expr-of "<schema>
             <element name=\"mytype\">
	             <simpleType>
	              <restriction base=\"integer\">
			            <minInclusive value=\"36\"/>
			            <maxInclusive value=\"42\"/>
			          </restriction>
	             </simpleType>
             </element>
				     <element name=\"myref\">
					     <complexType>
					       <sequence>
					         <element ref=\"mytype\"/>
					       </sequence>
					     </complexType>
				     </element>
           </schema>"))



(deftest test-complex-content
  (let [types (assoc predef-types "personinfo" (fn [value types attributes elements] (if value [true [[true "Donald" :firstname][true "Duck" :lastname]]] [:sequence [:firstname :lastname]])))
        schema (validation-fn-of 
         "<complexContent>        
         <extension base=\"personinfo\">
			      <sequence>
			        <element name=\"address\" type=\"string\"/>
			        <element name=\"city\" type=\"string\"/>
			        <element name=\"country\" type=\"string\"/>
			        <element name=\"zip\" type=\"byte\"/>
			      </sequence>
			    </extension>
          </complexContent>")]
    (is (= [:sequence [:firstname :lastname :address :city :country :zip]] (schema nil types {} {})))
    (is (= [true [[true "Broadway" :address] [true "New" :city] [true "USA" :country] [true 123 :zip] [true "Donald" :firstname] [true "Duck" :lastname]]] (schema (:content 
                 (parse-str  "<employee>
                              <address>Broadway</address>
                              <city>New York</city>
                              <country>USA</country>
                              <firstname>Donald</firstname>
                              <lastname>Duck</lastname>
                              <zip>123</zip>
                            </employee>")) types {} {})))
    
    (is (= [true [[true "Broadway" :address] [true "LA" :city] [true "USA" :country] [true 123 :zip] [true "Donald" :firstname] [true "Duck" :lastname]]] (schema (:content 
                 (parse-str  "<employee>
                              <address>Broadway</address>
                              <city>LA</city>
                              <country>USA</country>
                              <firstname>Donald</firstname>
                              <lastname>Duck</lastname>
                              <zip>123</zip>
                            </employee>")) types {} {})))
    
    (is (= [false [[true "Donald" :firstname] [true "Duck" :lastname]]] (schema (:content 
                   (parse-str  "<employee>
                              <address>Broadway</address>
                              <city>LA</city>
                              <country>USA</country>
                              <firstname>Donald</firstname>
                              <lastname>Duck</lastname>
                            </employee>")) types {} {})))
    
   (is (= [true [[true "Broadway" :address] [true "LA" :city] [true "USA" :country] [false 128 :zip] [true "Donald" :firstname] [true "Duck" :lastname]]] (schema (:content 
                 (parse-str  "<employee>
                              <address>Broadway</address>
                              <city>LA</city>
                              <country>USA</country>
                              <firstname>Donald</firstname>
                              <lastname>Duck</lastname>
                              <zip>128</zip>
                            </employee>")) types {} {})))
  ))


(deftest test-complex-content-with-element
  (let [text "
			<schema><complexType name=\"personinfo\">
			  <sequence>
			    <element name=\"firstname\" type=\"string\"/>
			    <element name=\"lastname\" type=\"string\"/>
			  </sequence>
			</complexType>
			
			<complexType name=\"fullpersoninfo\">
			  <complexContent>
			    <extension base=\"personinfo\">
			      <sequence>
			        <element name=\"address\" type=\"string\"/>
			        <element name=\"city\" type=\"string\"/>
			        <element name=\"country\" type=\"string\"/>
			      </sequence>
			    </extension>
			  </complexContent>
			</complexType>
			<element name=\"employee\" type=\"fullpersoninfo\"/>
      </schema>"]
       (let [res (decode text "<employee>
                          <address>Broadway</address>
                          <city>New York</city>
                          <country>USA</country>
                          <firstname>Donald</firstname>
                          <lastname>Duck</lastname>
                        </employee>")]
         (is (= [:employee [:address "Broadway"], [:city "New"], [:country "USA"], [:firstname "Donald"], [:lastname "Duck"]] res))
         (is (= true (-> res meta :employee))))
       (let [res (decode text "<employee>
                          <city>New York</city>
                          <country>USA</country>
                          <firstname>Donald</firstname>
                          <lastname>Duck</lastname>
                        </employee>")]
         (is (= [:employee [:firstname "Donald"], [:lastname "Duck"]] res))
         (is (= false (-> res meta :employee)))
         (is (= false (valid? res))))
       
       ))

(deftest test-complex-inline
  (let [schema  
    [:schema 
     [:element {:name "nameList"} 
      [:complexType 
       [:sequence 
        [:element {:name "name" :type "string"}]]]]]] 
    (is (= [:nameList [:name "anders"]] (decode schema [:nameList [:name "anders"]])))
    (is (= true (valid? (decode schema [:nameList [:name "anders"]]))))
  ))

(deftest test-nested-complex-inline
  (let [schema 
        [:schema 
         [:element {:name "part1"}
          [:complexType
           [:sequence
            [:element {:name "nameList"}
             [:complexType
              [:sequence
               [:element {:name "name"}
                [:simpleType
                  [:union
                   [:restriction {:base "string"}
                    [:enumeration {:value "small"}]
                    [:enumeration {:value "medium"}]
                    [:enumeration {:value "large"}]]]]]]]]]]]]]
    
     (is (= [:part1 [:nameList [:name "small"]]] (decode schema [:part1 [:nameList [:name "small"]]])))
     (is (= true (valid? (decode schema [:part1 [:nameList [:name "small"]]]))))
     (is (= [:part1 [:nameList [:name "asdf"]]] (decode schema [:part1 [:nameList [:name "asdf"]]])))
     (is (= false (valid? (decode schema [:part1 [:nameList [:name "asdf"]]]))))
     ))
     
  
(deftest test-attributes
  #_(let [schema
         [:schema 
          [:element {:name "part1"}
           [:complexType
            [:attribute {:name "attr1" :type "integer"}]
            [:attribute {:name "attr2" :type "string"}]]]]]
    
     (is (= [:part1 {:attr1 2, :attr2 "hej"}] (decode schema [:part1 {:attr1 2, :attr2 "hej"}])))
     ))


