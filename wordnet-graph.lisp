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

(defparameter *word-en* (make-hash-table :test #'equal))
(defparameter *relations* (make-hash-table :test #'equal))
(defparameter *inverse-relations* (make-hash-table :test #'equal))
(defparameter *properties* '("wn30_instanceOf" "wn30_hyponymOf"))

(defparameter *vertices* nil)
(defparameter *in-degree-fn* nil)
(defparameter *out-degree-fn* nil)

(defun relation (synset)
  (remove-duplicates (flatten
                      (mapcar (lambda (p)
                                (gethash synset (gethash p *relations*)))
                              *properties*))
		     :test #'equal))

(defun inverse-relation (synset)
  (remove-duplicates (flatten
                      (mapcar (lambda (p)
                                (gethash synset (gethash p *inverse-relations*)))
                              *properties*))
		     :test #'equal))
  
(defun word (synset)
  (car (gethash synset *word-en*)))

(defun init-hash-tables ()
  (dolist (p *properties*)
    (setf (gethash p *relations*) (make-hash-table :test #'equal))
    (setf (gethash p *inverse-relations*) (make-hash-table :test #'equal))))

(defun compute-degrees ()
  (multiple-value-bind (in-degree out-degree) (degrees *vertices* #'relation)
    (setf *in-degree-fn* in-degree)
    (setf *out-degree-fn* out-degree)))

(defun add-word-en (synset words)
  (setf (gethash synset *word-en*)
        (append words (gethash synset *word-en*))))

(defun add-relation (from to-list relation-name)
  (let ((hash-table (gethash relation-name *relations*)))
    (setf (gethash from hash-table) 
          (append to-list (gethash from hash-table)))))

(defun add-inverse-relation (from to-list relation-name)
  (let ((hash-table (gethash relation-name *inverse-relations*)))
    (dolist (n to-list)
      (setf (gethash n hash-table) 
            (push from (gethash n hash-table))))))

(defun read-wn ()
  (init-hash-tables)
  (let ((docs (gethash "docs" (gethash "response" (yason:parse (merge-pathnames "wn.json" *basedir*))))))
    (dolist (d docs)
      (let ((synset (gethash "doc_id" d)))
	(dolist (property-name *properties*)
	  (let ((property-value (gethash property-name d)))
            (when property-value
              (add-relation synset property-value property-name)
              (add-inverse-relation synset property-value property-name))))
        (add-word-en synset (gethash "word_en" d))
	(push synset *vertices*))))
  (compute-degrees))

;; transitive-reduction

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
		    (inverse-relation n)))
       nil)))))

(defun create-hierarchy (synsets)
  (let* ((s* (transitive-closure synsets #'relation)))
    (mapcar (lambda (n) (get-subtree-node n s*)) (roots s*))))

(defun debug-hierarchy (tree &optional (indent 0))
  (dolist (r tree)
    (format t "~a[~a] ~a~%" (make-string indent :initial-element #\space) indent (cdr (assoc :word r)))
    (debug-hierarchy (cdr (assoc :children r)) (+ 2 indent))))

(defun debug-hierarchy2 (tree &optional (indent 0))
  (with-output-to-string (str)
    (debug-hierarchy3 tree str indent)))

(defun debug-hierarchy3 (tree stream &optional (indent 0))
  (dolist (r tree)
    (format stream "~a[~a] ~a~%" (make-string indent :initial-element #\space) indent (cdr (assoc :word r)))
    (debug-hierarchy3 (cdr (assoc :children r)) stream (+ 2 indent))))
