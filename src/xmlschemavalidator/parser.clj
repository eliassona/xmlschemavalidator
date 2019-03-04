(ns xmlschemavalidator.parser
  (:use [clojure.pprint])
  (:require [instaparse.core :as insta]
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


(def schema
  (-> "<schema>
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
    </schema>" parse-str element->hiccup))




(def parser 
  (insta/parser "
                 SCHEMA = OPEN-PAREN ':schema' SPACE ((TYPES OPTIONAL-SPACE)* | TYPES) CLOSE-PAREN
                 TYPES = (SIMPLETYPE | COMPLEXTYPE | ELEMENT)
                 ELEMENT = OPEN-PAREN ':element' SPACE NAME-TYPE-ATTR CLOSE-PAREN
                 COMPLEXTYPE = OPEN-PAREN ':complexType' SPACE NAME-ATTR SPACE COMPLEXTYPE-BODY CLOSE-PAREN
                 COMPLEXTYPE-BODY = SEQUENCE | ALL | GROUP | CHOICE
                 GROUP = OPEN-PAREN ':group' SPACE ELEMENT* CLOSE-PAREN
                 ALL = OPEN-PAREN ':all' SPACE ELEMENT* CLOSE-PAREN
                 CHOICE = OPEN-PAREN ':choice' SPACE ELEMENT* CLOSE-PAREN
                 SEQUENCE = OPEN-PAREN ':sequence' SPACE ELEMENT* CLOSE-PAREN
                 SIMPLETYPE = (OPEN-PAREN ':simpleType' (SIMPLETYPE-BODY | (SPACE  (NAME-TYPE-ATTR | (NAME-ATTR SPACE SIMPLETYPE-BODY)))) CLOSE-PAREN)
                 NAME-ATTR = OPEN-BRACKET ':name' SPACE SYMBOL CLOSE-BRACKET
                 NAME-TYPE-ATTR = OPEN-BRACKET ':name' SPACE SYMBOL <','> SPACE ':type' SPACE SYMBOL CLOSE-BRACKET
                 SIMPLETYPE-BODY = [ANNOTATION] (UNION | RESTRICTION)
                 UNION = OPEN-PAREN ':union' [SPACE OPEN-BRACKET ':memberTypes' SPACE MEMBERTYPES CLOSE-BRACKET] SIMPLETYPE* CLOSE-PAREN
                 MEMBERTYPES = <'\"'> (NAME SPACE)* NAME <'\"'> 
                 ANNOTATION = OPEN-PAREN CLOSE-PAREN
                 
                 RESTRICTION = OPEN-PAREN ':restriction' SPACE OPEN-BRACKET ':base' SPACE SYMBOL CLOSE-BRACKET SPACE (RESTRICTION-BODY SPACE)* RESTRICTION-BODY CLOSE-PAREN
                 RESTRICTION-BODY = OPEN-PAREN RESTRICTION-KWS SPACE OPEN-BRACKET ':value' SPACE STRING CLOSE-BRACKET CLOSE-PAREN
                 RESTRICTION-KWS = ':enumeration' | ':minInclusive' | ':maxInclusive'
                 OPEN-PAREN = <'('>
                 CLOSE-PAREN = <')'>
                 OPEN-BRACKET = <'{'>
                 CLOSE-BRACKET = <'}'>
						     SYMBOL = <'\"'> #'[a-zA-Z_]'  #'\\w'* <'\"'>
                 NAME = #'[a-zA-Z_]'  #'\\w'*
                 STRING = <'\\\"'> #'([^\"\\\\]|\\\\.)*' <'\\\"'>
						     <SPACE> = <#'[ \t\n,]+'>
                 <OPTIONAL-SPACE> = <#'[ \t\n]*'>

"))



