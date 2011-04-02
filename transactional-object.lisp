(in-package :transactional-object)

(defvar *transaction* nil "The current transaction")

(defclass transaction ()
  ((objects :initform (make-hash-table)
	    :accessor objects
	    :documentation "A hash table with a copy of touched objects")
   (parent :initarg :parent
	   :reader parent
	   :initform nil
	   :documentation "The parent transaction. Think of nested transactions semantics!!")
   (status :initform :new
	   :accessor status)
   ))

(defmethod commited-p ((transaction transaction))
  (equalp (status transaction) :commited))

(define-condition transaction-conflict-error (serious-condition)
  ())

(define-condition versioning-error (transaction-conflict-error)
  ())

;; (defclass transactional-variable ()
;;   ((name :initarg :name
;; 	 :reader name
;; 	 :initform (error "Provide the variable name"))
;;    (value :initarg :value
;; 	  :reader value
;; 	  :initform (error "Provide the variable value")))
;;   (:metaclass transactional-class))

;; (defmethod (setf value) (value (stm-var stm-var))
;;   "Sets the value of a transactional variable in the transaction.
;;    If the transaction is nil, ."
;;   (when (not *transaction*)
;;     (error 'no-transaction-error))
;;   (let* ((ref (var stm-var))
;; 	 (old-value (value ref)))
;;     (add-change *transaction* stm-var value)))

(defun object-version (object)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha1 
    (flexi-streams:with-output-to-sequence (s)
      (cl-store:store object s)))))

(defun transactional-object-version (object)
  (let ((*transaction* nil))
    (object-version object)))

(defclass transactional-object ()
  ((%lock :initform (sb-thread:make-mutex)
	  :accessor %lock)))

(defun copy-transactional-object (origin target)
  (let ((*transaction* nil))
    (copy-object origin target)))

(defclass transactional-class (standard-class)
  ())

(defun transaction-object-copy (object &optional (transaction *transaction*))
  (multiple-value-bind (object-copy-entry found-p)
      (gethash object (objects transaction))
    (if found-p
	(cdr object-copy-entry)
	(let ((new-copy
	       (copy-transactional-object object
					  (allocate-instance (class-of object)))))
	  (setf (gethash object (objects transaction))
		(cons (transactional-object-version object) new-copy))
	  new-copy))))

(defmethod slot-value-using-class ((class transactional-class)
				   (instance transactional-object)
				   slot-def)
  "Get the slot value from the current transaction."
  (if *transaction*
      (let ((name (slot-definition-name slot-def))
	    (object-copy (transaction-object-copy instance)))
	(let ((*transaction* nil))
	  (slot-value object-copy name)))
      (call-next-method)))

(defmethod (setf slot-value-using-class) (new-value (class transactional-class)
					  (instance transactional-object)
					  slot-def)
  "Set the slot value in the transaction."
  (if *transaction*
      (let ((*transaction* nil))
	(sb-thread:with-mutex ((%lock instance))
	  (let ((name (slot-definition-name slot-def))
		(object-copy (transaction-object-copy instance)))
	    (setf (slot-value object-copy name)
		  new-value))))
      (call-next-method)))

;; (defmethod slot-boundp-using-class ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
;;   "Checks if the slot exists in the database."
;;   (when instance
;;     (let ((name (slot-definition-name slot-def)))
;;       (persistent-slot-boundp (get-con instance) instance name))))

;; (defmethod slot-boundp-using-class ((class persistent-metaclass) (instance persistent-object) (slot-name symbol))
;;   "Checks if the slot exists in the database."
;;   (loop for slot in (class-slots class)
;;      for matches-p = (eq (slot-definition-name slot) slot-name)
;;      until matches-p
;;      finally (return (if (and matches-p
;; 			      (subtypep (type-of slot) 'persistent-slot-definition))
;;                        (persistent-slot-boundp (get-con instance) instance slot-name)
;; 		       (call-next-method)))))

;; (defmethod slot-makunbound-using-class ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
;;   "Removes the slot value from the database."
;;   (persistent-slot-makunbound (get-con instance) instance (slot-definition-name slot-def)))

;; ================================================
;; METACLASS INITIALIZATION 
;; ================================================

(defmethod initialize-instance :around ((class transactional-class) &rest initargs
					&key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'transactional-object)))
      ;; 'transactional-object is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'transactional-object is not one of the superclasses, so we have to add it
      (apply #'call-next-method class :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'transactional-object)))
	     initargs)))

(defmethod reinitialize-instance :around ((class transactional-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed
      ;; this is exactly like above
      (if (loop for class in direct-superclasses
	     thereis (subtypep class (find-class 'transactional-object)))
	  (call-next-method)
	  (apply #'call-next-method class :direct-superclasses
		 (append direct-superclasses
			 (list (find-class 'my-object)))
		 initargs))
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method))) 

(defun superclass-member-p (class superclasses)
  "Searches superclass list for class"
  (some #'(lambda (superclass)
	    (or (eq class superclass)
		(let ((supers (class-direct-superclasses superclass)))
		  (when supers
		    (superclass-member-p class supers)))))
	superclasses))

(defun ensure-class-inherits-from (class from-classnames direct-superclasses)
  (let* ((from-classes (mapcar #'find-class from-classnames))
	 (has-persistent-objects 
	  (every #'(lambda (class) (superclass-member-p class direct-superclasses))
		 from-classes)))
    (if (not (or (member class from-classes) has-persistent-objects))
	(progn
	  (dolist (class from-classes)
	    (setf direct-superclasses (remove class direct-superclasses)))
	  (append direct-superclasses from-classes))
	direct-superclasses)))

(defmethod validate-transaction ((transaction transaction))
  (let ((*transaction* nil))
    (loop for (version . object-copy) being the hash-values of (objects transaction)
       using (hash-key object)
       when (not (equalp (transactional-object-version object) version))
       do (error "~A is not valid ~A ~A" transaction (object-version object) version))))

(defmethod commit-transaction ((transaction transaction))
  (let ((*transaction* nil))
    (loop for object being the hash-keys of (objects transaction)
       do
	 (sb-thread:grab-mutex (%lock object)))
    (unwind-protect
	 (progn
	   (validate-transaction transaction)
	   (loop for (version . object-copy) being the hash-values of (objects transaction)
	      using (hash-key object)
	      do (copy-transactional-object object-copy object))
	   (setf (status transaction) :commited))
      (loop for object being the hash-keys of (objects transaction)
	 do
	   (sb-thread:release-mutex (%lock object))))))

(defmethod rollback-transaction ((transaction transaction))
  (setf (objects transaction) (make-hash-table)))

(defmethod validate-superclass ((class transactional-class)
				(superclass standard-class))
  t)

(defmacro with-transaction ((&optional (var '*transaction*))
			    &body body)
  `(let ((,var (make-instance 'transaction)))
     ,@body
     (commit-transaction ,var)))

(defmethod serializable-slots-using-class ((object t) (class transactional-class))
  (remove '%lock (call-next-method) :key (lambda (obj)
					   (slot-value obj 'sb-pcl::name))))

(defmethod copyable-slots-using-class ((object t) (class transactional-class))
  (remove '%lock (call-next-method) :key (lambda (obj)
					   (slot-value obj 'sb-pcl::name))))