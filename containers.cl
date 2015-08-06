(defpackage container
  (:export container
	   container-get
	   container-set))

(declaim
 (optimize
  (compilation-speed 0)
  (debug 0)
  (safety 0)
  (space 2)
  (speed 3)))

(defclass container ()
  ((contents :initform (make-hash-table :test 'equal)
	     :type (hash-table symbol container-slot))))

(defclass container-slot ()
  ((value :accessor container-slot-value
	  :initarg :value)
   (getter :accessor container-slot-getter
	   :initform #'identity
	   :type function)
   (setter :accessor container-slot-setter
	   :initform #'identity
	   :type function)
   (metadata :accessor container-slot-metadata
	     :initform (make-instance 'container)
	     :type container)))

(defgeneric container-exists (c k))

(defmethod container-exists ((c container) (k symbol))
  (if (gethash k (slot-value c 'contents))
      t
      nil))

(defgeneric container-get (c k))

(defmethod container-get ((c container) (k symbol))
  (let ((this-container-slot (gethash k (slot-value c 'contents))))
    (let ((this-container-slot-value (slot-value this-container-slot 'value))
	  (this-container-slot-getter (the function (slot-value this-container-slot 'getter))))
      (funcall this-container-slot-getter this-container-slot-value))))

(defgeneric container-set (c k v))

(defmethod container-set ((c container) (k symbol) v)
  (if (container-exists c k)
      (setf (slot-value (gethash k (slot-value c 'contents)) 'value) v)
      (setf (gethash k (slot-value c 'contents))
	    (make-instance 'container-slot :value v))))
