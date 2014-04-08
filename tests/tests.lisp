(in-package :tx-object-tests)

(defun run-tests ()
  (fiveam:run 'transactional-object-tests))

(def-suite transactional-object-tests)

(in-suite transactional-object-tests)

(defclass person ()
  ((first-name :initarg :first-name
	       :accessor first-name)
   (last-name :initarg :last-name
	      :accessor last-name))
  (:metaclass transactional-class))

;; Transaction interleaving

(test transaction-conflict-test
  (let ((p (make-instance 'person)))
    (let ((t1 (make-transaction))
	  (t2 (make-transaction)))
      (let ((*transaction* t1))
	(setf (first-name p) "Mar"))
      (let ((*transaction* t2))
	(setf (first-name p) "Ar"))
      (commit-transaction t1)
      (signals error
	(commit-transaction t2)))))

;; unwinding

(test transaction-unwinding-test
  (let ((p (make-instance 'person :first-name "Mariano")))
    (block nil
      (with-transaction ()
	(setf (first-name p) "Martin")
	(return-from nil)))
    (is (equalp (first-name p) "Mariano"))))

;; transaction nesting
;; TODO: implement nested transactions
(test transaction-nesting-test
  (let ((p (make-instance 'person :first-name "Mariano")))
    (with-transaction ()
      (setf (first-name p) "Martin")
      (block nil
	(with-transaction ()
	  (setf (first-name p) "Marcos")
	  (return-from nil))))
    (is (equalp (first-name p) "Martin")))

  (let ((p (make-instance 'person :first-name "Mariano")))
    (with-transaction ()
      (setf (first-name p) "Martin")
      (with-transaction ()
	(setf (first-name p) "Marcos")))
    (is (equalp (first-name p) "Marcos"))))

;; no transaction test
(test no-transaction-test
  (let ((p (make-instance 'person :first-name "Mariano")))
    (setf (first-name p) "Martin")
    (is (equalp (first-name p) "Martin"))))

;; required transaction test
(defclass client (person)
  ((email :initarg :email
	  :initform nil
	  :accessor email))
  (:metaclass transactional-class
	      :requires-transaction nil))

(test requires-transaction-test
  (let ((p (make-instance 'client :first-name "Mariano")))
    (signals error
      (setf (first-name p) "Martin"))))
