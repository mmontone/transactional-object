(in-package :transactional-object)

(defclass person ()
  ((first-name :initarg :first-name
	       :accessor first-name)
   (last-name :initarg :last-name
	      :accessor last-name))
  (:metaclass transactional-class))
  

