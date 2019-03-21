(ns xmlschemavalidator.parser_test
  (:use [clojure.pprint])
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [clojure.data.xml :refer [parse-str parse sexp-as-element emit-str indent-str]]
            [xmlschemavalidator.core :refer [predef-types dbg apply-of]]
            [xmlschemavalidator.parser :refer :all]))


(defn assert-parsing [& texts]
  (doseq [[start t] texts]
    (let [res (-> t xml->hiccup (parser :start start))]
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

(deftest test-length-restriction
  (let [f(validation-fn-of [:restriction {:base "string"}
                            [:length {:value 10}]] :RESTRICTION)]
    (is (= [true "1234567890"] (f "1234567890" predef-types {} {})))
    (is (= [false "123456789"] (f "123456789" predef-types {} {})))
    (is (= [false "1234567890a"] (f "1234567890a" predef-types {} {})))
    ))

(deftest test-pattern-restriction
  (let [f(validation-fn-of [:restriction {:base "string"}
                       [:pattern {:value "[A-Z][A-Z][A-Z]"}]] :RESTRICTION)]
    (is (= [true "ABC"] (f "ABC" predef-types {} {})))
    (is (= [true "XYZ"] (f "XYZ" predef-types {} {})))
    (is (= [false "aBC"] (f "aBC" predef-types {} {})))
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
    (is (= [true [true 1 :seq1] [true "adsf" :seq2]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>adsf</seq2></udr>")) predef-types {} {})))
    (is (= [false [false 2 :seq2] [false "asdf" :seq1]] (f (:content (parse-str "<udr><seq2>2</seq2><seq1>asdf</seq1></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1] [false 2 :seq2] [false :undefined :seq3]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1] [false :undefined :seq3]] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))

(deftest test-all 
  (let [f (validation-fn-of 
          "<all>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </all>" :ALL)]
    (is (= [true [true 1 :seq1][true "asdf" :seq2]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>asdf</seq2></udr>")) predef-types {} {})))
    (is (= [true [true "asdf" :seq2][true 10 :seq1]] (f (:content (parse-str "<udr><seq2>asdf</seq2><seq1>10</seq1></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1] [false 2 :seq2] [false :undefined :seq3]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1] [false :undefined :seq3]] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))
(deftest test-choice 
  (let [f (validation-fn-of 
          "<choice>
		         <element name=\"seq1\" type=\"integer\"/>
		         <element name=\"seq2\" type=\"string\"/>
		       </choice>" :CHOICE)]
    (is (= [false [true 1 :seq1] [false 2 :seq2]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2></udr>")) predef-types {} {})))
    (is (= [false [false 2 :seq2] [true 1 :seq1]] (f (:content (parse-str "<udr><seq2>2</seq2><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [true [true 1 :seq1]] (f (:content (parse-str "<udr><seq1>1</seq1></udr>")) predef-types {} {})))
    (is (= [true [true "asdf" :seq2]] (f (:content (parse-str "<udr><seq2>asdf</seq2></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1] [false 2 :seq2] [false :undefined :seq3]] (f (:content (parse-str "<udr><seq1>1</seq1><seq2>2</seq2><seq3>2</seq3></udr>")) predef-types {} {})))
    (is (= [false [true 1 :seq1] [false :undefined :seq3]] (f (:content (parse-str "<udr><seq1>1</seq1><seq3>1</seq3></udr>")) predef-types {} {})))
    ))


(deftest test-complex-type
  (let [e (validation-expr-of 
            "<complexType name=\"cp\">
              <sequence>
                <element name=\"seq1\" type=\"string\"/>
                <element name=\"seq2\" type=\"string\"/>
              </sequence>
              <attribute name=\"country\" type=\"string\"/>
              <attribute name=\"zip\" type=\"integer\"/>
             </complexType>" :COMPLEXTYPE)]
    (is (= :type (-> e meta :kind)))
    (is (= [true #{[true [true "usa" :country] [true 13672 :zip]]} 
            [true [true "hej" :seq1] [true "bla" :seq2]]] 
           ((-> e eval first val) (content-of (sexp-as-element [:udr {:country "usa", :zip 13672} [:seq1 "hej"][:seq2 "bla"]])) predef-types {} {})))
    
    ))

(deftest test-inline-complex-type
  (let [e (validation-expr-of 
            [:complexType 
            [:attribute {:name "attr1" :type "byte"}]] :COMPLEXTYPE)]
    (is (= nil (-> e meta :kind)))
    (is (= [true #{[true [true 10 :attr1]]}] ((eval e) (content-of (sexp-as-element [:udr {:attr1 10}])) predef-types {} {})))
    (is (= [true #{[true [false 128 :attr1]]}] ((eval e) (content-of (sexp-as-element [:udr {:attr1 128}])) predef-types {} {})))
))    
            

(deftest test-simple-element
  (let [f (validation-fn-of
            [:schema [:element {:name "hej" :type "int"}]])]
    (is (= [true 0 :hej] (f (sexp-as-element [:hej 0]) predef-types {} {})))
    (is (= [true 100 :hej] (f (sexp-as-element [:hej 100]) predef-types {} {})))
    (is (= [false 2147483648 :hej] (f (sexp-as-element [:hej 2147483648]) predef-types {} {})))
    (let [res (decode [:schema [:element {:name "hej" :type "int"}]] [:hej 0])]
      (is (= [:hej 0] res))
      (is (= {:hej true} (meta res))))
    )
  )


(deftest test-element-with-complex-type
  (let [he [:schema 
            [:complexType {:name "cp"}
             [:sequence
              [:element {:name "test", :type "byte"}]]]
            [:element {:name "udr", :type "cp"}]]
       f (validation-fn-of he)]                                      
    (is (= [true #{[true]} [true [true 0 :test]] :udr] (f (sexp-as-element [:udr [:test 0]]) predef-types {} {})))
    (is (= [true #{[true]} [true [true 36 :test]] :udr] (f (sexp-as-element [:udr [:test 36]]) predef-types {} {})))
    (is (= [true #{[true]} [true [false 128 :test]] :udr] (f (sexp-as-element [:udr [:test 128]]) predef-types {} {})))
    (is (= [true #{[true]} [true [false -129 :test]] :udr] (f (sexp-as-element [:udr [:test -129]]) predef-types {} {})))
    (let [res (decode he [:udr [:test 0]])]
      (is (= [:udr [:test 0]] res))
      (is (= {:udr true} (meta res)))
      (is (= {:test true} (meta (second res))))
      )
    
    ))

(deftest test-element-with-union-type
  (let [f (validation-fn-of
              [:schema
               [:simpleType {:name "aunion"}
                [:union {:memberTypes "byte"}
                 [:simpleType
                  [:restriction {:base "string"}
                   [:enumeration {:value "small"}]
                   [:enumeration {:value "medium"}]
                   [:enumeration {:value "large"}]]]]]
               [:element {:name "udr", :type "aunion"}]])]
    (is (= [true 10 :udr] (f (sexp-as-element [:udr 10]) predef-types {} {})))
    (is (= [false 128 :udr] (f (sexp-as-element [:udr 128]) predef-types {} {})))
    (is (= [true "small" :udr] (f (sexp-as-element [:udr "small"]) predef-types {} {})))
    (is (= [false "asdf" :udr] (f (sexp-as-element [:udr "asdf"]) predef-types {} {})))    
   ))
    
(deftest test-inline-element-with-one-attr
  (let [f (validation-fn-of
            [:schema 
             [:element {:name "udr"} 
              [:complexType 
               [:attribute {:name "attr1" :type "byte"}]]]])]
    (is (= [true #{[true [true 10 :attr1]]} :udr] (f (sexp-as-element [:udr {:attr1 10}]) predef-types {} {})))
    ))

(deftest test-inline-element-with-two-attrs
  (let [f (validation-fn-of
            [:schema 
             [:element {:name "udr"} 
              [:complexType 
               [:attribute {:name "attr1" :type "byte"}]
               [:attribute {:name "attr2" :type "string"}]]]])]
    (is (= [true #{[true [true 10 :attr1][true "hej" :attr2]]} :udr] (f (sexp-as-element [:udr {:attr1 10, :attr2 "hej"}]) predef-types {} {})))
    ))

(deftest test-inline-element-with-seq
  (let [f (validation-fn-of
              [:schema 
               [:element {:name "udr"} 
                [:complexType
                 [:sequence
                  [:element {:name "seq1" :type "positiveInteger"}]
                  ]]]])]
      (is (= [true #{[true]} [true [true 1 :seq1]] :udr] (f (sexp-as-element [:udr [:seq1 1]]) predef-types {} {})))
      ))

(deftest test-inline-element-with-seq-and-two-attrs
  (let [schema [:schema 
               [:element {:name "udr"} 
                [:complexType
                 [:sequence
                  [:element {:name "seq1" :type "positiveInteger"}]
                  ]
                 [:attribute {:name "attr1" :type "byte"}]
                 [:attribute {:name "attr2" :type "string"}]]]]
        f (validation-fn-of schema)]
      (is (= [true #{[true [true 10 :attr1][true "hej" :attr2]]} [true [true 1 :seq1]] :udr] 
             (f (sexp-as-element [:udr {:attr1 10, :attr2 "hej"} [:seq1 1]]) predef-types {} {})))
      (let [res (decode schema [:udr {:attr1 10, :attr2 "hej"} [:seq1 1]])]
        (is (= [:udr {:attr1 10, :attr2 "hej"} [:seq1 1]] res))
        (is (= [:udr {:attr1 {:value 10, :status true} :attr2 {:value "hej", :status true}} [:seq1 {:value 1, :status true}]] (with-status res)))
        )
      ))

 
(deftest test-ref
  (let [he [:schema 
            [:element {:name "theref"} 
             [:complexType
              [:sequence
               [:element {:name "seq1" :type "positiveInteger"}]
               ]]]
            [:element {:name "udr"}
               [:complexType
                [:all 
                 [:element {:ref "theref"}]]]]]
        f (validation-fn-of he)]
    (is (= [true #{[true]} [true [true #{[true]} [true [true 1 :seq1]] :theref]] :udr] (f (sexp-as-element [:udr [:theref [:seq1 1]]]) predef-types {} {})))
    (let [res (decode he [:udr [:theref [:seq1 1]]])]
      (is (= [:udr [:theref [:seq1 1]]] res))
      (is (= {:udr true} (meta res)))
      (is (= {:theref true} (meta (second res))))
      (is (= {:seq1 true} (meta (second (second res)))))
      (is (= [:udr [:theref [:seq1 {:value 1, :status true}]]] (with-status res)))
      ) 
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
                   [:simpleType
                    [:restriction {:base "string"}
                     [:enumeration {:value "small"}]
                     [:enumeration {:value "medium"}]
                     [:enumeration {:value "large"}]]]]]]]]]]]]]]
    
     (is (= [:part1 [:nameList [:name "small"]]] (decode schema [:part1 [:nameList [:name "small"]]])))
     (is (= true (valid? (decode schema [:part1 [:nameList [:name "small"]]]))))
     (is (= [:part1 [:nameList [:name "asdf"]]] (decode schema [:part1 [:nameList [:name "asdf"]]])))
     (is (= false (valid? (decode schema [:part1 [:nameList [:name "asdf"]]]))))
     ))
