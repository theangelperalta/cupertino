;;;; package.lisp
;;;; Swift Build (SWBBuildService) interception package.
;;;;
;;;; Cupertino interposes itself between xcodebuild and the real
;;;; SWBBuildService by pointing SWBBUILDSERVICE_PATH at its own (Mach-O)
;;;; image.  The proxy forwards every wire frame verbatim in both
;;;; directions and, best-effort, sniffs the service->client stream for the
;;;; handful of build-operation events the dashboard consumes.

(defpackage :swb
  (:use #:cl)
  (:local-nicknames (#:mpk #:messagepack)
                    (#:flex #:flexi-streams))
  (:export ;; framing
           #:read-frame
           #:write-frame
           #:swb-framing-error
           ;; events
           #:frame->event
           #:write-event
           #:read-event
           #:*interesting-messages*
           ;; proxy
           #:run-proxy
           #:+service-path-env+
           #:+real-service-env+
           #:+events-path-env+
           #:+trace-env+))
