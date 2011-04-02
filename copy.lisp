(in-package :transactional-object)

(defun copy-object (origin target)
  (assert (equalp (class-of origin) (class-of target)))
  (loop for slot in (copyable-slots origin)
     for slot-name = (slot-value slot 'sb-pcl::name)
     when (slot-boundp origin slot-name)
     do (setf (slot-value target slot-name)
	      (slot-value origin slot-name)))
  target)

(defgeneric copyable-slots (object)
  (declare (optimize speed))
  (:documentation 
   "Return a list of slot-definitions to serialize. The default
    is to call copyable-slots-using-class with the object 
    and the objects class")
  (:method ((object standard-object))
   (copyable-slots-using-class object (class-of object)))
#+(or sbcl cmu openmcl allegro)
  (:method ((object structure-object))
   (copyable-slots-using-class object (class-of object)))
  (:method ((object condition))
   (copyable-slots-using-class object (class-of object))))

; unfortunately the metaclass of conditions in sbcl and cmu 
; are not standard-class

(defgeneric copyable-slots-using-class (object class)
  (declare (optimize speed))
  (:documentation "Return a list of slot-definitions to serialize.
   The default calls compute slots with class")
  (:method ((object t) (class standard-class))
   (class-slots class))
#+(or sbcl cmu openmcl allegro) 
  (:method ((object t) (class structure-class))
   (class-slots class))
#+sbcl
  (:method ((object t) (class sb-pcl::condition-class))
   (class-slots class))
#+cmu
  (:method ((object t) (class pcl::condition-class))
   (class-slots class)))
