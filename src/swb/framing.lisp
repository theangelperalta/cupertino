;;;; framing.lisp
;;;;
;;;; Wire framing for the SWBBuildService protocol, reverse-engineered from
;;;; captured Xcode 26 traffic.  Each message on the pipe (both directions)
;;;; is:
;;;;
;;;;   [ uint64 LE: channel id ][ uint32 LE: body length ][ body bytes ]
;;;;
;;;; The body is a MessagePack string (the message name) followed by the
;;;; MessagePack-encoded arguments.  This file only deals with the framing
;;;; envelope; bodies are handled opaquely so forwarding stays byte-exact
;;;; even when the message *schemas* change between Xcode releases.

(in-package :swb)

(define-condition swb-framing-error (error)
  ((detail :initarg :detail :reader swb-framing-error-detail))
  (:report (lambda (c s)
             (format s "SWB framing error: ~a" (swb-framing-error-detail c)))))

;; A sane upper bound on a single body so a corrupt length field can't make
;; us allocate gigabytes.  Real frames are tiny; PIF transfers are the
;; largest and stay well under this.
(defparameter +max-body-length+ (* 512 1024 1024))

(defun %read-le-uint (stream nbytes &key eof-ok)
  "Read an NBYTES little-endian unsigned integer from STREAM.
Returns NIL on a clean EOF before the first byte when EOF-OK is true;
otherwise signals SWB-FRAMING-ERROR on a truncated read."
  (let ((first (read-byte stream nil :eof)))
    (when (eq first :eof)
      (if eof-ok
          (return-from %read-le-uint nil)
          (error 'swb-framing-error :detail "EOF reading integer")))
    (let ((value first))
      (loop for i from 1 below nbytes
            for b = (read-byte stream nil :eof)
            do (when (eq b :eof)
                 (error 'swb-framing-error :detail "EOF mid-integer"))
               (setf value (logior value (ash b (* 8 i)))))
      value)))

(defun %write-le-uint (stream value nbytes)
  "Write VALUE as an NBYTES little-endian unsigned integer to STREAM."
  (dotimes (i nbytes)
    (write-byte (ldb (byte 8 (* 8 i)) value) stream)))

(defun read-frame (stream)
  "Read one wire frame from binary STREAM.
Returns (values CHANNEL BODY) where BODY is an (unsigned-byte 8) vector, or
NIL on a clean EOF at a frame boundary.  Signals SWB-FRAMING-ERROR on a
truncated or implausible frame."
  (let ((channel (%read-le-uint stream 8 :eof-ok t)))
    (unless channel
      (return-from read-frame nil))
    (let ((len (%read-le-uint stream 4)))
      (when (> len +max-body-length+)
        (error 'swb-framing-error
               :detail (format nil "implausible body length ~d" len)))
      (let* ((body (make-array len :element-type '(unsigned-byte 8)))
             (got (read-sequence body stream)))
        (unless (= got len)
          (error 'swb-framing-error
                 :detail (format nil "short body: got ~d of ~d" got len)))
        (values channel body)))))

(defun write-frame (stream channel body)
  "Write a frame with CHANNEL and BODY (octet vector) to binary STREAM.
Reproduces the exact bytes that READ-FRAME consumed, so forwarding is
byte-for-byte transparent at the transport layer."
  (%write-le-uint stream channel 8)
  (%write-le-uint stream (length body) 4)
  (write-sequence body stream))
