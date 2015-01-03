(defpackage #:optima.hash-pattern
  (:use #:cl #:optima #:optima.core #:alexandria)
  (:export #:hashtable #:hash-property))

(in-package #:optima.hash-pattern)

(defun %gethash (item table)
  "Safe gethash, returns values as a list"
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (hash-table-p table)
      (multiple-value-list (gethash item table))
      '(nil nil)))

(defstruct (hash-property-pattern (:include constructor-pattern)
                                  (:constructor make-hash-property-pattern
                                                (item value-pattern
                                                      &aux (subpatterns (list value-pattern)))))
  item)

(defun hash-property-pattern-value-pattern (pattern)
  (first (constructor-pattern-subpatterns pattern)))

(defmethod constructor-pattern-destructor-sharable-p ((x hash-property-pattern) (y hash-property-pattern))
  (eq (hash-property-pattern-item x) (hash-property-pattern-item y)))

(defmethod constructor-pattern-make-destructor ((pattern hash-property-pattern) var)
  (with-slots (item) pattern
    (with-unique-names (it)
      (make-destructor :bindings `((,it (%gethash ,item ,var)))
                       :predicate-form `(second ,it)
                       :accessor-forms (list `(first ,it))))))

(defmethod parse-constructor-pattern ((name (eql 'hash-property)) &rest args)
  (destructuring-bind (item pattern) args
    (make-hash-property-pattern item (parse-pattern pattern))))

(defmethod unparse-pattern ((pattern hash-property-pattern))
  `(hash-property ,(hash-property-pattern-item pattern)
                  ,(unparse-pattern (hash-property-pattern-value-pattern pattern))))

(defpattern hashtable (&rest args)
  "Syntax:
(hashtable {KEY PATTERN}*)

Expansion:
(hashtable {k p}*) => (and (hash-property k p)*)

Example:
(let ((tab (make-hashtable)))
  (setf (gethash :name tab) \"John\")
  (setf (gethash :age tab) 23)
  (match tab
    ((hashtable :name \"John\" :age age) age)))
=> 23
"
  (when args
    `(and ,@(loop for (key pattern &rest _) on args by #'cddr
               collect (list 'hash-property key pattern)))))
