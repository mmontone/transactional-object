(defpackage transactional-object
  (:nicknames :tx-object)
  (:use :cl :sb-mop :cl-store)
  (:export #:transactional-class
	   #:with-transaction
	   #:make-transaction
	   #:start-transaction
	   #:*transaction*
	   #:commit-transaction
	   #:rollback-transaction))