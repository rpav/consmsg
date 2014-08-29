(in-package :consmsg)

(defclass message-pool ()
  ((path-to-id :initform (make-hash-table :test 'equal))
   (id-to-path :initform (make-hash-table))
   (max-id :initform 0)
   (registered-objects :initform (make-hash-table))
   (wildcard-objects :initform (make-hash-table :test 'equal))
   (global-wildcard-objects :initform nil)))

(defmethod initialize-instance ((pool message-pool) &key &allow-other-keys)
  (call-next-method))

(defgeneric ensure-path (pool path))
(defgeneric find-path (pool path))
(defgeneric broadcast (pool path msg &optional sender))
(defgeneric ensure-broadcast (pool path msg &optional sender))
(defgeneric register (pool path object))
(defgeneric register-wildcard (pool wildcard object))
(defgeneric apply-all-wildcards (pool path))
(defgeneric apply-wildcard (pool wildcard path objects))
(defgeneric receive-message (object path msg)
  (:method :around (object path msg)
    (let ((*receiver-object* object))
      (call-next-method object path msg))))

(defmethod ensure-path ((pool message-pool) path)
  (declare (type list path))
  (with-slots (path-to-id id-to-path max-id) pool
    (if-let (id (gethash path path-to-id))
      id
      (let ((id (incf max-id)))
        (setf (gethash path path-to-id) id)
        (setf (gethash id id-to-path) (copy-seq path))
        (apply-all-wildcards pool path)
        id))))

(defmethod find-path ((pool message-pool) (path list))
  (with-slots (path-to-id) pool
    (gethash path path-to-id)))

(defmethod find-path ((pool message-pool) (path integer))
  (with-slots (id-to-path) pool
    (gethash path id-to-path)))

(defmethod broadcast ((pool message-pool) (id integer) msg
                      &optional (sender *receiver-object*))
  (with-slots (id-to-path registered-objects) pool
    (when-let ((object-list (gethash id registered-objects))
               (path (gethash id id-to-path)))
      (dolist (o object-list)
        (unless (eq o sender)
          (receive-message o path msg)))
      t)))

(defmethod broadcast ((pool message-pool) (path list) msg
                      &optional (sender *receiver-object*))
  (when-let ((id (find-path pool path)))
    (broadcast pool id msg sender)))

(defmethod ensure-broadcast ((pool message-pool) (path list) msg
                             &optional (sender *receiver-object*))
  (let ((id (ensure-path pool path)))
    (broadcast pool id msg sender)))

(defmethod register ((pool message-pool) (path integer) object)
  (with-slots (id-to-path registered-objects) pool
    (if (gethash path id-to-path)
        (pushnew object (gethash path registered-objects))
        (error "Path ID not registered: ~A" path))))

(defmethod register ((pool message-pool) (path list) object)
  (if (wildcardp path)
      (register-wildcard pool path object)
      (let ((id (ensure-path pool path)))
        (register pool id object))))

(defmethod register ((pool message-pool) (path (eql '*)) object)
  (register-wildcard pool path object))

(defmethod register-wildcard :after ((pool message-pool) wildcard object)
  (with-slots (path-to-id) pool
    (dolist (path (hash-table-keys path-to-id))
      (apply-wildcard pool wildcard path (list object)))))

(defmethod register-wildcard ((pool message-pool) (wildcard (eql '*)) object)
  (with-slots (global-wildcard-objects) pool
    (pushnew object global-wildcard-objects)))

(defmethod register-wildcard ((pool message-pool) (wildcard list) object)
  (with-slots (wildcard-objects) pool
    (pushnew object (gethash wildcard wildcard-objects))))

(defmethod apply-all-wildcards ((pool message-pool) path)
  (with-slots (global-wildcard-objects wildcard-objects path-to-id)
      pool
    (maphash (lambda (wildcard objects)
               (apply-wildcard pool wildcard path objects))
             wildcard-objects)
    (apply-wildcard pool '* path global-wildcard-objects)))

(defmethod apply-wildcard ((pool message-pool) (wildcard list) (path list)
                           objects)
  (let ((id (find-path pool path)))
    (when (wildcard-match wildcard path)
      (dolist (o objects) (register pool id o)))))

(defmethod apply-wildcard ((pool message-pool) (wildcard (eql '*)) (path list)
                           objects)
  (let ((id (find-path pool path)))
    (dolist (o objects)
      (register pool id o))))

(defmethod receive-message ((object function) path msg)
  (funcall object path msg))
