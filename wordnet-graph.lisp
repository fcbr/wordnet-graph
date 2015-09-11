;;;; wordnet-graph.lisp

(in-package #:wordnet-graph)

(defparameter *basedir*
  (make-pathname :directory
		 (pathname-directory
		  (asdf:component-pathname (asdf:find-system '#:wordnet-graph)))))

(defparameter test-data
  '("15126931-n" "15124361-n"
    "02604760-v" "01158872-v" "00056930-v" "00024073-r"
    "14974264-n" "06468951-n" "14877585-n" "05996646-n" "01210854-a"
    "11495041-n" "04007894-n" "14966667-n"))

(defparameter *synsets* (make-hash-table :test #'equal))

(defparameter *word-en* (make-hash-table :test #'equal))
(defparameter *reverse-word-en* (make-hash-table :test #'equal))

(defparameter *forward-eges* (make-hash-table :test #'equal))
(defparameter *reverse-edges* (make-hash-table :test #'equal))

(defparameter *current-properties* nil)

(defparameter *hyp-inst-properties* '("wn30_instanceOf" "wn30_hyponymOf"))

(defparameter *all-properties* '(
                            "wn30_classifiedByRegion"
                            "wn30_classifiedByTopic"
                            "wn30_classifiedByUsage"
                            "wn30_classifiesByRegion"
                            "wn30_classifiesByTopic"
                            "wn30_classifiesByUsage"
                            "wn30_seeAlso"
                            "wn30_hasInstance"
                            "wn30_instanceOf"
                            "wn30_entails"
                            "wn30_similarTo"
                            "wn30_attribute"
                            "wn30_causes"
                            "wn30_hypernymOf"
                            "wn30_hyponymOf"
                            "wn30_memberHolonymOf"
                            "wn30_memberMeronymOf"
                            "wn30_partHolonymOf"
                            "wn30_partMeronymOf"
                            "wn30_sameVerbGroupAs"
                            "wn30_substanceHolonymOf"
                            "wn30_substanceMeronymOf"))

(defparameter *vertices* nil)
(defparameter *in-degree-fn* nil)
(defparameter *out-degree-fn* nil)

(defun word (synset)
  "Return one arbitrary word from the synset word_en field."
  (car (gethash synset *word-en*)))

(defun words (synset)
  "Return one arbitrary word from the synset word_en field."
  (gethash synset *word-en*))

(defun search-word (word)
  "Return all synsets that have word in their word_en field."
  (gethash word *reverse-word-en*))

(defun edges (synset &optional (properties *current-properties*))
  "Return all directed edges spanning from vertex."
  (remove-duplicates (flatten
                      (mapcar (lambda (p)
                                (gethash synset (gethash p *forward-eges*)))
                              properties))
		     :test #'equal))

(defun reverse-edges (synset &optional (properties *current-properties*))
  "Return all directed edges incoming on vertex."
  (remove-duplicates (flatten
                      (mapcar (lambda (p)
                                (gethash synset (gethash p *reverse-edges*)))
                              properties))
		     :test #'equal))
  
(defun all-bidirectional-edges (synset &optional (properties *current-properties*))
  "Return all bidirectional edges spanning from vertex.  Meaning, if
  there is an edge from ID to ID' but not from ID' to ID, then ID' is
  not included in the result."
  (remove-if (lambda (r)
               (not (member synset (edges r properties) :test #'equal)))
             (edges synset properties)))

(defun get-synset (id)
  (gethash id *synsets*))

(defun isolated-vertices ()
  (let ((vertices *vertices*)
        (result nil))
    (multiple-value-bind (in-fn out-fn)
        (degrees vertices  #'edges)
      (dolist (v vertices)
        (let ((in (funcall in-fn v))
              (out (funcall out-fn v)))
          (when (= 0 (+ in out))
            (let* ((synset (get-synset v))
                   (gloss (car (getf synset :|gloss_en|))))
              (push (list :id v :gloss gloss) result))))))
    result))

(defun search-paths (w1 w2)
  (when (and w1 w2)
    (let ((w1-synsets (search-word w1))
          (w2-synsets (search-word w2))
          (result nil))
      (when (and w1-synsets w2-synsets)
        (dolist (s1 w1-synsets)
          (let ((prev (dijkstra s1 *vertices* #'edges)))
            (dolist (s2 w2-synsets)
              (let ((path (reconstruct-path prev s2)))
                (when path
                  (push s1 path)
                  (push (list :id1 s1 :id2 s2 :path path) result))))))
        (let* ((sorted-result
                (sort result #'< :key (lambda (x) (length (getf x :path)))))
               (len (length sorted-result)))
          (subseq sorted-result 0 (if (< len 10) len 10)))))))

(defparameter *clique-cache* nil)

(defun find-cliques ()
  (maximal-cliques *vertices* #'all-bidirectional-edges
                   (lambda (c)
                     (when (> (length c) 2)
                       (push c *clique-cache*)))))

(defun compute-degrees ()
  (multiple-value-bind (in-degree out-degree) (degrees *vertices* #'edges)
    (setf *in-degree-fn* in-degree)
    (setf *out-degree-fn* out-degree)))

(defun add-word-en (synset words)
  (setf (gethash synset *word-en*)
        (append words (gethash synset *word-en*)))
  (dolist (w words)
    (setf (gethash w *reverse-word-en*)
          (push synset (gethash w *reverse-word-en*)))))

(defun add-edge (from to-list relation-name)
  (let ((hash-table (gethash relation-name *forward-eges*)))
    (setf (gethash from hash-table) 
          (append to-list (gethash from hash-table)))))

(defun add-reverse-edge (from to-list relation-name)
  (let ((hash-table (gethash relation-name *reverse-edges*)))
    (dolist (n to-list)
      (setf (gethash n hash-table) 
            (push from (gethash n hash-table))))))

(defun read-wn (filename properties)
  (setf *current-properties* properties)
  (dolist (p properties)
    (setf (gethash p *forward-eges*) (make-hash-table :test #'equal))
    (setf (gethash p *reverse-edges*) (make-hash-table :test #'equal)))
  (let ((docs (gethash "docs" (gethash "response" (yason:parse (merge-pathnames filename *basedir*))))))
    (dolist (d docs)
      (let ((synset (gethash "doc_id" d)))
        (setf (gethash synset *synsets*) d)
	(dolist (property-name properties)
	  (let ((property-value (gethash property-name d)))
            (when property-value
              (add-edge synset property-value property-name)
              (add-reverse-edge synset property-value property-name))))
        (add-word-en synset (gethash "word_en" d))
	(push synset *vertices*))))
  (compute-degrees))

;; wn.json *hyp-inst-properties*
(defun read-hyp-inst-wn ()
  (read-wn #p"wn-hi.json" *hyp-inst-properties*))

(defun read-all-wn ()
  (read-wn #p"wn.json" *all-properties*))

(defun read-hypernym-wn ()
  (read-wn #p"wn.json" '("wn30_hypernymOf")))

(defun roots (synsets)
  (remove-if-not (lambda (s) 
		   (zerop (funcall *out-degree-fn* s))) 
		 synsets))

(defun get-subtree (n closure)
  "Given a node N and its closure CLOSURE, this returns an association
list with the vertices in CLOSURE in hierarchical order.  This doesn't
deal with cycles yet."
  (when (find n closure :test #'equal)
    (acons 
     :word (word n)
     (acons 
      :synset n
      (acons 
       :children
       (remove nil (mapcar (lambda (n) (get-subtree-node n closure)) 
                           (reverse-edges n)))
       nil)))))

(defun transitive-closure (synsets)
  (transitive-closure synsets #'edges))

(defun create-hierarchy (synsets)
  (let* ((s* (transitive-closure synsets #'edges)))
    (mapcar (lambda (n) (get-subtree-node n s*)) (roots s*))))

(defun debug-hierarchy (tree &optional (indent 0))
  (dolist (r tree)
    (format t "~a[~a] ~a~%" (make-string indent :initial-element #\space) indent (cdr (assoc :word r)))
    (debug-hierarchy (cdr (assoc :children r)) (+ 2 indent))))
