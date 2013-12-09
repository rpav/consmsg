(defpackage #:consmsg
  (:use #:cl #:alexandria #:trivial-channels.queue #:trivial-channels)
  (:export
   #:message-pool #:thread-pool

   #:ensure-path #:find-path
   #:ensure-broadcast #:broadcast
   #:register
   #:receive-message

   #:thread-pool-start #:thread-pool-stop #:with-thread-pool))
