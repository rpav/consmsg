(in-package :consmsg)

(defclass thread-pool (message-pool)
  ((threads :initform nil)
   (lock :initform (bt:make-recursive-lock "Threadpool"))
   (channel :initform (make-channel))))

(defgeneric thread-pool-stop (pool))
(defgeneric thread-pool-start (pool))

(defun thread-pool-thread (channel lock id-to-path registered-objects)
  (loop as msg = (recvmsg channel)
        until (eq 'quit msg)
        do (let ((id (car msg))
                 (msg (cadr msg))
                 (sender (caddr msg)))
             (bt:acquire-lock lock t)
             (when-let ((object-list (gethash id registered-objects))
                        (path (gethash id id-to-path)))
               (bt:release-lock lock)
               (dolist (o object-list)
                 (handler-case
                     (unless (eq o sender)
                       (receive-message o path msg))
                   (error (e)
                     (format *debug-io* "THREAD-POOL Error: ~A~%" e))))))))

(defun thread-pool-kill-threads (channel thread-array)
  (loop for i from 0 below (length thread-array) do
    (sendmsg channel 'quit)))

(defmethod initialize-instance ((pool thread-pool) &key (max-threads 2)
                                &allow-other-keys)
  (call-next-method)
  (with-slots (threads) pool
    (setf threads (make-array max-threads :initial-element nil))))

(defmethod thread-pool-start ((pool thread-pool))
  ;; In case it isn't obvious, we can't use WITH-SLOTS, which expands
  ;; to (slot-value pool ...).  This would retain a reference in the
  ;; thread and never allow collection.  Thus we get the values first
  ;; instead.
  (let ((threads (slot-value pool 'threads))
        (channel (slot-value pool 'channel))
        (lock (slot-value pool 'lock))
        (id-to-path (slot-value pool 'id-to-path))
        (registered-objects (slot-value pool 'registered-objects)))
    (loop for i from 0 below (length threads)
          do (setf (aref threads i)
                   (bt:make-thread
                    (lambda ()
                      (thread-pool-thread channel lock
                                          id-to-path
                                          registered-objects))
                    :name "consmsg-threadpool-thread")))
    ;; Note that as of writing this will never be triggered in SBCL or
    ;; CCL, at least.  You must rely on WITH-THREAD-POOL, or manually
    ;; stop the pool with THREAD-POOL-STOP.
    (tg:finalize pool
                 (lambda ()
                   (thread-pool-kill-threads channel threads)))))

(defmethod thread-pool-stop ((pool thread-pool))
  (with-slots (channel threads) pool
    (thread-pool-kill-threads channel threads))
  (tg:cancel-finalization pool))

(defmacro call-next-method-locked (pool)
  `(with-slots (lock) ,pool
     (bt:with-recursive-lock-held (lock)
       (call-next-method))))

(defmethod broadcast ((pool thread-pool) (id integer) msg
                      &optional sender)
  (with-slots (channel) pool
    (sendmsg channel (list id msg sender))))

(defmethod ensure-path :around ((pool thread-pool) path)
  (call-next-method-locked pool))

(defmethod find-path :around ((pool thread-pool) path)
  (call-next-method-locked pool))

(defmethod register :around ((pool thread-pool) path object)
  (call-next-method-locked pool))

(defmacro with-thread-pool ((var &key (max-threads 2)) &body body)
  `(let ((,var (make-instance 'thread-pool :max-threads ,max-threads)))
     (unwind-protect
          (progn
            (thread-pool-start ,var)
            ,@body)
       (thread-pool-stop ,var))))
