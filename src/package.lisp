(in-package :defpackage+-user-1)
(defpackage+ #:consmsg
  (:use #:cl #:alexandria #:trivial-channels.queue #:trivial-channels)
  (:export
   #:message-pool #:thread-pool

   #:ensure-path #:find-path
   #:ensure-broadcast #:broadcast
   #:register #:unregister
   #:receive-message

   #:*receiver-object*

   #:thread-pool-start #:thread-pool-stop #:with-thread-pool))
(in-package :consmsg)

 ;; Variables

(defvar *receiver-object* nil)
