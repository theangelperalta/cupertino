;;;; proxy.lisp
;;;;
;;;; The interception proxy.  When Cupertino launches xcodebuild it sets
;;;; SWBBUILDSERVICE_PATH to its own image and passes, via the environment,
;;;; the path of the real SWBBuildService (and optionally an events sink).
;;;; xcodebuild then spawns *us* as the build service; we re-spawn the real
;;;; service and shuttle wire frames between xcodebuild (our stdin/stdout)
;;;; and the service, sniffing the service->client direction for events.
;;;;
;;;; Transparency contract: every frame is forwarded byte-for-byte; sniffing
;;;; is best-effort and can never alter or stall the forwarded stream.

(in-package :swb)

(defparameter +service-path-env+ "SWBBUILDSERVICE_PATH"
  "Apple-defined env var xcodebuild reads to discover the Swift Build service
binary. Cupertino points it at its own image to install the proxy.")

(defparameter +real-service-env+ "CUPERTINO_SWB_REAL"
  "Env var holding the path of the genuine SWBBuildService executable.
Its presence is what puts Cupertino into proxy mode.")

(defparameter +events-path-env+ "CUPERTINO_SWB_EVENTS"
  "Env var holding a path the proxy appends decoded events to (optional).")

(defparameter +trace-env+ "CUPERTINO_SWB_TRACE"
  "Env var that, when set to a non-empty value, makes the proxy emit a
synthetic :UNKNOWN event for every wire message name not in
*INTERESTING-MESSAGES*. Off by default to keep the events file lean.")

(defun %binary-fd-stream (fd direction)
  "An unbuffered binary stream over raw file descriptor FD."
  #+sbcl
  (sb-sys:make-fd-stream fd
                         :input (eq direction :input)
                         :output (eq direction :output)
                         :element-type '(unsigned-byte 8)
                         :buffering :none)
  #-sbcl
  (error "swb proxy requires SBCL fd streams"))

(defun %forward (in out &optional sniff)
  "Forward wire frames from IN to OUT until EOF.
Each frame is written and flushed before SNIFF (if any) runs, so sniffing
can never delay forwarding.  A framing error stops this direction but is
not re-signalled past the thread."
  (handler-case
      (loop
        (multiple-value-bind (channel body) (read-frame in)
          (unless channel (return))
          (write-frame out channel body)
          (finish-output out)
          (when sniff
            (ignore-errors (funcall sniff channel body)))))
    (swb-framing-error ()
      ;; Transport-level desync: stop forwarding this direction cleanly.
      nil)
    (error () nil))
  (ignore-errors (finish-output out)))

(defun run-proxy (real-service &optional events-path)
  "Run as the SWBBuildService proxy in front of REAL-SERVICE.
Returns the real service's exit code.  If EVENTS-PATH is non-NIL, decoded
service->client events are appended to it as readable forms."
  (let* ((client-in  (%binary-fd-stream 0 :input))
         (client-out (%binary-fd-stream 1 :output))
         (trace-env (uiop:getenv +trace-env+))
         (trace-unknown (and trace-env (plusp (length trace-env))))
         (events (when events-path
                   (ignore-errors
                    (open events-path :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create
                                      :element-type 'character
                                      :external-format :utf-8))))
         (sniff (when events
                  (lambda (channel body)
                    (let ((event (frame->event channel body
                                               :trace-unknown trace-unknown)))
                      (when event (write-event events event))))))
         (proc (uiop:launch-program (list real-service)
                                    :input :stream
                                    :output :stream
                                    :error-output :interactive ; logs -> our stderr
                                    :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let* ((svc-in  (uiop:process-info-input proc))
                (svc-out (uiop:process-info-output proc))
                ;; client -> service: forward request frames (no sniff).
                (up (bt:make-thread
                     (lambda ()
                       (%forward client-in svc-in)
                       (ignore-errors (close svc-in)))
                     :name "swb-proxy-c2s"))
                ;; service -> client: forward and sniff.
                (down (bt:make-thread
                       (lambda ()
                         (%forward svc-out client-out sniff))
                       :name "swb-proxy-s2c")))
           (bt:join-thread down)   ; service closed its output: build done
           (bt:join-thread up)
           (uiop:wait-process proc))
      (when events (ignore-errors (close events))))))
