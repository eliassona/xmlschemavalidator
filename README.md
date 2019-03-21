# xmlschemavalidator


A XML Schema decoder and validator. 
Work in progress!!

## Usage 

```clojure
(ns example.core
  (:require [xmlschemavalidator.parser :refer [decode valid? with-status]]))
  
```

Define an XML schema

```clojure
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
     <complexType name=\"cp\">
       <sequence>
         <element name=\"uniontest\" type=\"theunion\"/>
       </sequence>
     </complexType>
		<element name=\"udr\" type=\"cp\">
		  </element>
    </schema>")
```

And decode...

```clojure
=> (decode schema "<udr><uniontest>0</uniontest></udr>")
[:udr [:uniontest 0]]

```
To find out if the decode is valid

```clojure
=> (meta (decode schema "<udr><uniontest>0</uniontest></udr>"))
{:udr true} ;udr level is valid

=> (-> (decode schema "<udr><uniontest>0</uniontest></udr>") second meta)
{:uniontest false} ;uniontest level invalid due to the intrange type and its restriction 
```

You could pre-compile the schema by applying partial on the schema

```clojure
=> (def d (partial decode schema))
=> (d "<udr><uniontest>0</uniontest></udr>")
[:udr [:uniontest 0]]
```
Check the whole decoding was valid

```clojure
=> (valid? (d "<udr><uniontest>0</uniontest></udr>"))
false
```

Let's decode something that is valid

```clojure
=> (valid? (d "<udr><uniontest>medium</uniontest></udr>")) ; it's valid due to the stringenum type and it enumeration restriction
true
```

You could also use the sexp-as-element format instead of xml strings as arguments to decode.

```clojure
=> (valid? (d [:udr [:uniontest "medium"]]))
true
```

You could decorate the decoded data with the status with with-status
```clojure
=> (with-status (d [:udr [:uniontest "medium"]]))
[:udr [:uniontest {:value "medium", :status true}]]

```

## The following tags have not been implemented yet (and maybe more)
* simpleContext
* complexContent
* attributeGroup
* list


## License

Copyright © 2019 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
