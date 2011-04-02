(in-package :transactional-object)

(defclass person ()
  ((first-name :initarg :first-name
	       :accessor first-name)
   (last-name :initarg :last-name
	      :accessor last-name))
  (:metaclass transactional-class))

;; Transaction interleaving

;; (let ((t1 (make-instance 'transaction))
;;       (t2 (make-instance 'transaction)))
;;   (let ((*transaction* t1))
;;     (setf (first-name *p*) "Mar"))
;;   (let ((*transaction* t2))
;;     (setf (first-name *p*) "Ar"))
;;   (commit-transaction t1)
;;   (commit-transaction t2))


;; unwinding

;; (block nil
;;   (with-transaction ()
;;     (setf (last-name *p*) "Martina") (return-from nil)))

