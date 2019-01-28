(ns xmlschemavalidator.core-test
  (:require [clojure.test :refer :all]
            [xmlschemavalidator.core :refer :all]))



(defn assert-decode [expected-meta expected-data actual-data]
  (is (= expected-data actual-data))
  (is (= expected-meta (meta actual-data))))

(deftest test-valid-primitive-type
  (let [schema
    "<schema>    
			<element name=\"lastname\" type=\"string\"/>
     </schema>"]
    (assert-decode {:lastname :ok} 
                   {:lastname "Refsnes"} ((parse-schema schema) 
                                            "<lastname>Refsnes</lastname>"))
    ))

(deftest test-default-attr
  #_(let [schema "<schema><element name=\"color\" type=\"string\" default=\"red\"/></schema>"]
     (assert-decode {:color :ok} {:color "black"} ((parse-schema schema) "<color>black</color>"))
     (assert-decode {:color :ok} {:color "red"} ((parse-schema schema) ""))
     )
  )
(deftest test-fixed-attr
  #_(let [schema "<schema><element name=\"color\" type=\"string\" fixed=\"red\"/></schema>"]
    (assert-decode {:color "color must be red"} {:color "black"} ((parse-schema schema) "<color>black</color>"))
    (assert-decode {:color :ok} {:color "red"} ((parse-schema schema) "<color>red</color>"))
    )
  )


(deftest test-valid-simple-schema
  #_(let [schema "<schema>
							    <element name=\"size\">
								    <simpleType>
								      <restriction base=\"integer\">
								        <enumeration value=\"small\"/>
								        <enumeration value=\"medium\"/>
								        <enumeration value=\"large\"/>
								      </restriction>
								    </simpleType>
							    </element>
							  </schema>"]
    (assert-decode {:size :ok} {:size "small"} ((parse-schema schema) "<size>small</size>"))
    (assert-decode {:size :ok} {:size "medium"} ((parse-schema schema) "<size>medium</size>"))
    (assert-decode {:size :ok} {:size "large"} ((parse-schema schema) "<size>large</size>"))))


(deftest test-invalid-data-simple-schema
  #_(let [schema "<schema>
							    <element name=\"size\">
								    <simpleType>
								      <restriction base=\"integer\">
								        <enumeration value=\"small\"/>
								        <enumeration value=\"medium\"/>
								        <enumeration value=\"large\"/>
								      </restriction>
								    </simpleType>
							    </element>
							  </schema>"]
    (assert-decode {:size "invalid is not part of enumeration"} {:size "invalid"} ((parse-schema schema) "<size>invalid</size>"))
    (assert-decode {:size "invalidsize doesn't exist in schema"} {:size "invalid"} ((parse-schema schema) "<invalidsize>small</invalidsize>"))
    ))


(deftest test-valid-simple-complexType
  #_(let [schema 
    "<xs:complexType name=\"lettertype\" mixed=\"true\">
      <xs:sequence>
        <xs:element name=\"name\" type=\"xs:string\"/>
        <xs:element name=\"orderid\" type=\"xs:positiveInteger\"/>
        <xs:element name=\"shipdate\" type=\"xs:date\"/>
      </xs:sequence>
    </xs:complexType>"]))


(deftest test-valid-complexType-schema-with-sequence 
  #_(let [schema 
     "<element name=\"person\">
       <complexType>
         <sequence>
            <element name=\"full_name\" type=\"string\"/>
            <element name=\"child_name\" type=\"string\" maxOccurs=\"10\"/>
         </sequence>
      </complexType>
     </element>"]
    
  ))

