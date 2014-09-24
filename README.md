# consmsg

This is a message pool and dispatch system inspired in part by
[OSC](http://opensoundcontrol.org/).  Messages are broadcast to
*paths*, where objects *may* have been registered to receive them.
This is similar to the *observer model*.

```lisp
(let ((pool (make-instance 'consmsg-pool)))
  (register pool '(a b)
            (lambda (path msg) (format t "Message: ~A~%" msg)))
  (broadcast pool '(a b) "a message"))

;; =>
;; Message: a message
```

The above registers a function object to receive messages for the
*path* `(a b)`.  When a message is sent to this path, the function is
called with the path and the message.  The message can be any object.
*Delivery order is not guaranteed.*

*Wildcards* may also be registered:

```lisp
(let ((pool (make-instance 'consmsg-pool)))
  (register pool '(a *)
            (lambda (path msg) (format t "Message: ~A~%" msg)))
  (register pool '*
            (lambda (path msg) (format t "Global: ~A~%" msg)))
  (ensure-broadcast pool '(a b) "one")
  (ensure-broadcast pool '(a c) "two")
  (ensure-broadcast pool '(b c) "three"))

;; =>
;; Message: one
;; Global: one
;; Message: two
;; Global: two
;; Global: three
```

Finally, there is a *thread pool*, which implements a message pool
where messages are dispatched on threads:

```lisp
(with-thread-pool (pool)
  (register pool '(a *)
            (lambda (path msg)
              (format t "Path: ~A Message: ~A~%" path msg)))
  (register pool '*
            (lambda (path msg)
              (format t "Global: ~A ~A~%" path msg)))
  (ensure-broadcast pool '(a b) "hello")
  (ensure-broadcast pool '(a c) "world")
  (ensure-broadcast pool '(b c) "blah"))

;; =>
;; Global: Global: (A C) world
;; Global: Global: (A C) world
;; (A B) hello
;; Path: (A C) Message: world
;; Global: (A B) Message: hello
;; (B C) blah
```

Thus illustrating the necessity for proper locking.

## Basic usage

Generally, one registers an object with `REGISTER` and broadcasts
messages with `BROADCAST`.  A path must be a list of symbols, where
`COMMON-LISP:*` is a special symbol for *wildcard*, or the symbol
`COMMON-LISP:*` alone, which indicates the *global wildcard*.

```lisp
(register some-pool '(a path) some-object)
(broadcast some-pool '(a path) some-message)
```

It is possible to specify a *sender* to `BROADCAST`.  This is useful
if the receiver is *also* registered for the path, to avoid feedback
loops.  **This is now optional,** because `sender` now defaults to the
receiving object.  However, to illustrate:

```lisp
(let ((pool (make-instance 'message-pool)))
  (labels ((receiver1 (path msg)
             (format t "Receiver1 - Path: ~A Msg: ~A~%" path msg)
             (ensure-broadcast pool '(a path) "from receiver1"
                               #'receiver1)) ;; Optional, this is the default
           (receiver2 (path msg)
             (format t "Receiver2 - Path: ~A Msg: ~A~%" path msg)))
    (register pool '(a path) #'receiver1)
    (register pool '(a path) #'receiver2)
    (broadcast pool '(a path) "from nowhere")))

;; =>
;; Receiver2 - Path: (A PATH) Msg: from nowhere
;; Receiver1 - Path: (A PATH) Msg: from nowhere
;; Receiver2 - Path: (A PATH) Msg: from receiver1
```

Wildcards may be registered but *not* broadcast.  This is in no small
part for dispatch efficiency, but also see *Idiom* below.

```lisp
(register some-pool '(a * b *) some-object)
```

Messages broadcast to paths *not explicitly registered* are discarded.
This means that, by default, a wildcard will not receive messages!
You may explicitly register a path using `ENSURE-PATH`, or ensure the
path when broadcast using `ENSURE-BROADCAST`.  Note that every path
registered consumes a bit of memory, so it would be unwise to
`ENSURE-BROADCAST` arbitrary messages from untrusted sources (e.g.,
the network).

```lisp
(register some-pool '(a *) some-object)
(broadcast some-pool '(a b) "foo") ;; => nothing received by some-object!

(ensure-path some-pool '(a b))
(broadcast some-pool '(a b) "foo") ;; => "foo" now received by some-object
```

Specific paths (i.e., not wildcards) have an associated integer ID.
This ID may be used in place of a list-of-symbols path.  This will
shortcut the path ID being looked up, and may be useful for
high-volume broadcasts to the same ID, or for network protocols.

```lisp
(let ((path-id (find-path some-pool '(a b))))
  (broadcast some-pool path-id "message to (A B)"))
```

## `thread-pool`

The `thread-pool` implementation allows messages to be handled in
parallel.  While delivery order is not guaranteed for regular message
pools, this extends further in thread pools where execution order is
not guaranteed, and the same message may be handled by multiple
receivers at once.  Additionally, messages may be queued and not
executed until prior messages are handled.

When creating a thread pool, the `:max-threads` keyword may be
specified with an integer parameter.  This indicates the number of
threads which will be started by `THREAD-POOL-START`.  The default is
2.

The following protocol is added:

* `thread-pool-start POOL`: Start the threads in `POOL`.
* `thread-pool-stop POOL`: Stop the threads in `POOL`.  This may not
  be immediate.  Threads will not be interrupted, rather, a `quit`
  message is queued and the threads will exit once all prior queued
  messages are handled.  **This must be called to guarantee threads
  are stopped and `POOL` is collected.**

It should be noted that at the time of writing, at least for SBCL and
CCL, threads should be stopped manually due to potential GC issues
which may preclude collection of the thread-pool object.  To aid with
this, the following macro is available:

* `with-thread-pool (NAME &key (max-threads 2)) &body`: Create and
  implicitly start and stop a thread-pool, lexically bound to `NAME`.

Example:

```lisp
(with-thread-pool (pool)
  ;; pool is implicitly started
  (register pool '(a b) ...)
  (broadcast pool '(a b) ...))
;; pool is stopped now
```

## Idiom

Message pools are intended to be similar to the *observer model*, but
with further separation between the *observers* and *objects*.

Under the observer model, an observer registers interest in a specific
object, and receives notifications when that object changes.

Under the message pool model, an object registers interest in a
*path*, and any message to that *path* is received.  While observers
are *unidirectional* or perhaps *bidirectional*, message pools are
*omnidirectional*.  State changes may be *requested* or *observed* via
the same path and message.

For instance, one may have an irc listener which broadcasts a message
to `(chat-message irc)` that contains the detail of each message or
event.  Interested listeners, such as a UI, logging, or some other
processing mechanism, may register listeners for this path and thus
receive messages.  The irc listener may also listen on this path and
send any message it receives to the irc server.

## Networking

At present, no network integration exists; however, consmsg is not
specifically about *transporting* messages, but rather dispatching
them once received.

For instance, one could encode a message and path with
[conspack](https://github.com/conspack/cl-conspack) and send it with
[ZeroMQ](http://zeromq.org/) or similar.  When a message is received,
simply `BROADCAST` it.  Similarly, register a function with a global
wildcard to receive all messages to a pool, then encode and transmit
them.

## Protocol

The `CONSMSG` protocol is as follows:

* `ensure-path POOL PATH`: Ensure a specific path exists and all
  wildcards apply.  Return a path ID.
* `find-path POOL PATH`: Find the ID for a specific path, or `NIL` if
  the path has not been registered.
* `broadcast POOL PATH MESSAGE &optional SENDER`: Broadcast a message
  to `PATH`.  Every registered object should receive `MESSAGE`.  Order
  is not guaranteed.  `MESSAGE` is silently discarded if the path is
  not registered, or no objects are registered for the path.  If
  `SENDER` is specified, and `SENDER` is also registered for `PATH`,
  then it is specifically excluded from receiving `MESSAGE`.
* `ensure-broadcast POOL PATH MESSAGE &optional SENDER`: Broadcast a
  message to `PATH`.  Calls `ENSURE-PATH` and then `BROADCAST` on the
  ID.
* `register POOL PATH OBJECT`: Register `OBJECT` to receive messages
  on `PATH`.  Wildcards are permissible.  The same object may be
  registered multiple times, but should only receive messages once.
* `register-wildcard POOL WILDCARD OBJECT`: Register `OBJECT` on
  wildcard `WILDCARD`.  This is called by `REGISTER` internally and
  should generally not be called explicitly by the user.
* `unregister POOL PATH OBJECT`: Remove `OBJECT` from the path. It
  will no longer receive messages sent to `PATH` in `POOL`.
* `apply-all-wildcards POOL PATH`: Apply all known wildcards to the
  path `PATH`.  This may be called repeatedly on the same path, and
  should not result in duplicate messages to objects.  This is called
  internally by `ENSURE-PATH`, and should generally not be called
  explicitly by the user.
* `apply-wildcard POOL WILDCARD PATH OBJECTS`: Register `OBJECTS` on
  the specific path `PATH` if `WILDCARD` matches `PATH`.  This may be
  called repeatedly with the same parameters, and should not result in
  duplicate messages to objects.  This is called internally by
  `APPLY-ALL-WILDCARDS` and `REGISTER-WILDCARD`, and should generally
  not be called explicitly by the user.
* `receive-message OBJECT PATH MESSAGE`: Called when `MESSAGE` is
  received on `PATH`, when `OBJECT` has been registered on `PATH` or a
  matching wildcard (including the global wildcard).  Generally,
  specialize on `OBJECT`.  This is already implemented for `FUNCTION`,
  and the method for `FUNCTION` should not be altered.

Generally, the following generic functions are useful for day-to-day usage:

* `ensure-path`
* `find-path`
* `register`
* `unregister`
* `broadcast`
* `ensure-broadcast`

To implement a new receiver that can be used as the `OBJECT` parameter
to `REGISTER`, implement a method on the following:

* `receive-message`

The following generic functions are generally useful only when
implementing a new type of message-pool, e.g., see the implementation
of `thread-pool`:

* `register-wildcard`
* `apply-all-wildcards`
* `apply-wildcard`
