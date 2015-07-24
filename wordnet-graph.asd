;;;; wordnet-graph.asd

(asdf:defsystem #:wordnet-graph
  :description "Wordnet as a graph."
  :author "Fabricio Chalub <fcbrbr@gmail.com>"
  :license "See LICENSE file"
  :depends-on (#:drakma
	       #:graph-algorithms
	       #:alexandria
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "wordnet-graph")))

