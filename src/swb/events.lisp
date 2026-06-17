;;;; events.lisp
;;;;
;;;; Best-effort decoding of frame bodies into Cupertino build events.  Only
;;;; the message *name* (a leading MessagePack string) is needed to dispatch;
;;;; argument fields are decoded generically for now and carried through as
;;;; :ARGS, to be mapped onto dashboard fields once their layouts are pinned
;;;; against captured traffic.
;;;;
;;;; Everything here is wrapped so a decode failure never propagates into the
;;;; forwarding path -- a garbled or schema-changed message simply yields no
;;;; event, and the dashboard falls back to text parsing.

(in-package :swb)

;; Service->client message names we care about (confirmed in Xcode 26
;; traffic).  Anything else is forwarded but not turned into an event.
(defparameter *interesting-messages*
  '(("BUILD_OPERATION_STARTED"      . :build-started)
    ("BUILD_OPERATION_ENDED"        . :build-ended)
    ("BUILD_TARGET_STARTED"         . :target-started)
    ("BUILD_TARGET_ENDED"           . :target-ended)
    ("BUILD_TASK_STARTED"           . :task-started)
    ("BUILD_TASK_ENDED"             . :task-ended)
    ("BUILD_DIAGNOSTIC_EMITTED"     . :diagnostic)
    ("BUILD_CONSOLE_OUTPUT_EMITTED" . :console-output)
    ("BUILD_PROGRESS_UPDATED"       . :progress))
  "Alist mapping wire message names to Cupertino event-type keywords.")

(defun %decode-body (body)
  "Decode BODY into (values NAME ARGS).
NAME is the leading MessagePack string (or NIL if the body isn't
name-prefixed); ARGS is the list of remaining decoded MessagePack values."
  (let ((stream (flex:make-in-memory-input-stream body)))
    (let ((name (handler-case (mpk:decode-stream stream)
                  (error () nil))))
      (values name
              (when (stringp name)
                (loop for v = (handler-case (mpk:decode-stream stream)
                                (end-of-file () :eof)
                                (error () :eof))
                      until (eq v :eof)
                      collect v))))))

(defun frame->event (channel body)
  "Return a plist event for an interesting frame, or NIL.
Shape: (:TYPE keyword :NAME string :CHANNEL integer :ARGS list).  Never
signals: any decode error yields NIL."
  (handler-case
      (multiple-value-bind (name args) (%decode-body body)
        (when (stringp name)
          (let ((type (cdr (assoc name *interesting-messages* :test #'string=))))
            (when type
              (list :type type :name name :channel channel :args args)))))
    (error () nil)))

(defun write-event (stream event)
  "Serialize EVENT as one newline-terminated readable form to STREAM."
  (let ((*print-readably* nil)
        (*print-pretty* nil)
        (*print-circle* nil))
    (prin1 event stream)
    (terpri stream)
    (force-output stream)))

(defun read-event (stream)
  "Read one event form from STREAM, or :EOF at end of stream."
  (read stream nil :eof))
