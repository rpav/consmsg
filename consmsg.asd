(defpackage :consmsg.asdf
  (:use #:cl #:asdf))

(in-package :consmsg.asdf)

(defsystem :consmsg
  :description "Simple, efficient message pool"
  :author "Ryan Pavlik"
  :license "BSD-2-Clause"
  :version "0.0"

  :depends-on (:alexandria :bordeaux-threads :trivial-channels :defpackage-plus)
  :pathname "src"
  :serial t

  :components
  ((:file "package")
   (:file "match")
   (:file "message-pool")
   (:file "thread-pool")))
