(defpackage transactional-object
  (:use :cl :sb-mop :cl-store)
  (:export #:transactional-class
	   #:with-transaction
	   #:make-transaction
	   #:*transaction*
	   #:commit-transaction
	   #:rollback-transaction))
  