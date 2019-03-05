(ns xmlschemavalidator.parser_test
  (:require [clojure.test :refer :all]
            [instaparse.core :as insta]
            [clojure.data.xml :refer [parse-str parse sexp-as-element]]
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
   
   #_[:SCHEMA 
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
   
   
  ))