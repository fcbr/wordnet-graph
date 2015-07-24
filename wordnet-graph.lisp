;;;; wordnet-graph.lisp

(in-package #:wordnet-graph)

(defparameter test-data
    '("15124361-n" "02604760-v" "01158872-v" "00056930-v" "00024073-r" "14974264-n" "06468951-n"
;; "00047534-r" "11410625-n" "05660268-n" "08462320-n" "14845743-n" "00014285-r"
 "14877585-n" "05996646-n" "01210854-a" "11495041-n" "04007894-n" "14966667-n"))

(defparameter *relations* (make-hash-table :test #'equal))
(defparameter *properties* '("wn30_instanceOf" "wn30_hasInstance" "wn30_hyponymOf" "wn30_hypernymOf" "word_en"))

(defparameter *vertices* nil)
(defparameter *in-degree-fn* nil)
(defparameter *out-degree-fn* nil)

(defun relation-up (synset)
  (remove-duplicates (append 
		      (gethash synset (gethash "wn30_instanceOf" *relations*))
		      (gethash synset (gethash "wn30_hyponymOf" *relations*))
		      )
		     :test #'equal))

(defun relation-down (synset)
  (remove-duplicates (append 
		      (gethash synset (gethash "wn30_hasInstance" *relations*))
		      (gethash synset (gethash "wn30_hypernymOf" *relations*))
		      )
		     :test #'equal))

(defun word (synset)
  (car (gethash synset (gethash "word_en" *relations*))))

(defun init-hash-tables ()
  (dolist (p *properties*)
    (setf (gethash p *relations*) (make-hash-table :test #'equal))))

(defun compute-degrees ()
  (multiple-value-bind (in-degree out-degree) (degrees *vertices* #'relation-up)
    (setf *in-degree-fn* in-degree)
    (setf *out-degree-fn* out-degree)))

(defun read-wn ()
  (init-hash-tables)
  (let ((docs (gethash "docs" (gethash "response" (yason:parse #p"wn.json")))))
    (dolist (d docs)
      (let ((synset (gethash "doc_id" d)))
	(dolist (property-name *properties*)
	  (let ((property-value (gethash property-name d))
		(property-hash-table (gethash property-name *relations*)))
	    (when property-value
	      (setf (gethash synset property-hash-table) 
		    (append property-value (gethash synset property-hash-table))))))
	(push synset *vertices*))))
  (compute-degrees))

;; transitive-reduction

(defun transitive-closure (synsets)
  (let ((closure synsets)
	(tmp nil))
    (loop 
       (setf tmp (remove-duplicates 
		  (append closure (mapcan #'relation-up closure)) 
		  :test #'equal))
       (when (= (length tmp) (length closure))
	 (return))
       (setf closure tmp))
    (remove-duplicates closure :test #'equal)))

(defun roots (synsets)
  (remove-if-not (lambda (s) 
		   (zerop (funcall *out-degree-fn* s))) 
		 synsets))

(defun get-subtree-node (n closure)
  (when (find n closure :test #'equal)
    (acons 
     :word (word n)
     (acons 
      :synset n
      (acons 
       :children
       (remove nil (mapcar (lambda (n) (get-subtree-node n closure)) 
		    (relation-down n)))
       nil)))))

(defun create-hierarchy (synsets)
  (let* ((s* (transitive-closure synsets)))
    (mapcar (lambda (n) (get-subtree-node n s*)) (roots s*))))
