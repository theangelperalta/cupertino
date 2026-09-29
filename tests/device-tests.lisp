;;;; device-tests.lisp
;;;; Unit tests for the physical-device table (src/info/devices.lisp).

(in-package #:cupertino/tests)

;;; -------------------------------------------------------------------------
;;; Fixtures
;;; -------------------------------------------------------------------------

(defun parse-devicectl-json (json)
  "Parse JSON with the same yason settings LIST-DEVICE-INFO uses."
  (let ((yason:*parse-object-as* :hash-table)
        (yason:*parse-json-arrays-as-vectors* t))
    (yason:parse json)))

(defparameter *physical-device-json*
  "{\"identifier\": \"ABC-123\",
    \"deviceProperties\": {\"name\": \"Angel's iPhone\", \"osVersionNumber\": \"26.0\"},
    \"hardwareProperties\": {\"marketingName\": \"iPhone 17 Pro\",
                             \"productType\": \"iPhone18,1\",
                             \"reality\": \"physical\"},
    \"connectionProperties\": {\"tunnelState\": \"connected\",
                               \"transportType\": \"wired\",
                               \"potentialHostnames\": [\"abc.coredevice.local\", \"ABC-123.coredevice.local\"]}}")

(defparameter *simulated-device-json*
  "{\"identifier\": \"SIM-1\",
    \"deviceProperties\": {\"name\": \"Sim\"},
    \"hardwareProperties\": {\"reality\": \"simulated\"},
    \"connectionProperties\": {}}")

;;; -------------------------------------------------------------------------
;;; json-seq-first
;;; -------------------------------------------------------------------------

(deftest json-seq-first/handles-lists-vectors-and-empties
  (ok (equal "a" (cupertino::json-seq-first '("a" "b"))))
  (ok (equal "a" (cupertino::json-seq-first #("a" "b"))))
  (ok (null (cupertino::json-seq-first nil)))
  (ok (null (cupertino::json-seq-first #())))
  (ok (null (cupertino::json-seq-first (parse-devicectl-json "[]"))))
  ;; Empty adjustable vector with stale backing storage.
  (let ((v (make-array 4 :initial-element "stale" :fill-pointer 0)))
    (ok (null (cupertino::json-seq-first v)))))

;;; -------------------------------------------------------------------------
;;; physical-device-p
;;; -------------------------------------------------------------------------

(deftest physical-device-p/filters-only-simulated
  (ok (cupertino::physical-device-p (parse-devicectl-json *physical-device-json*)))
  (ok (not (cupertino::physical-device-p (parse-devicectl-json *simulated-device-json*))))
  (testing "reality under properties.hardware"
    (ok (not (cupertino::physical-device-p
              (parse-devicectl-json
               "{\"properties\": {\"hardware\": {\"reality\": \"simulated\"}}}")))))
  (testing "missing reality is treated as physical"
    (ok (cupertino::physical-device-p (parse-devicectl-json "{\"identifier\": \"X\"}")))))

;;; -------------------------------------------------------------------------
;;; device-table-row
;;; -------------------------------------------------------------------------

(deftest device-table-row/full-device
  (ok (equal '("Angel's iPhone" "26.0" "abc.coredevice.local" "ABC-123"
               "connected (wired)" "iPhone 17 Pro (iPhone18,1)" "")
             (cupertino::device-table-row (parse-devicectl-json *physical-device-json*)))))

(deftest device-table-row/sparse-device
  (testing "missing fields become empty cells rather than errors"
    (ok (equal '("" "" "" "D1" "" "" "")
               (finishes
                 (cupertino::device-table-row
                  (parse-devicectl-json "{\"identifier\": \"D1\"}"))))))
  (testing "empty potentialHostnames and product type only"
    (ok (equal '("" "" "" "D2" "" "iPad14,1" "")
               (cupertino::device-table-row
                (parse-devicectl-json
                 "{\"identifier\": \"D2\",
                   \"hardwareProperties\": {\"productType\": \"iPad14,1\"},
                   \"connectionProperties\": {\"potentialHostnames\": []}}"))))))

(deftest device-table-row/last-connection-date
  (let ((row (cupertino::device-table-row
              (parse-devicectl-json
               "{\"identifier\": \"D3\",
                 \"connectionProperties\": {\"lastConnectionDate\": \"2020-01-01T00:00:00.000Z\"}}"))))
    (ok (plusp (length (seventh row))))))

;;; -------------------------------------------------------------------------
;;; list-device-names
;;; -------------------------------------------------------------------------

(deftest list-device-names/drops-simulators-and-keeps-order
  (let* ((devices (parse-devicectl-json
                   (format nil "[~A, ~A, {\"identifier\": \"LAST\"}]"
                           *physical-device-json* *simulated-device-json*)))
         (rows (cupertino::list-device-names devices)))
    (ok (= 2 (length rows)))
    (ok (equal '("ABC-123" "LAST") (mapcar #'fourth rows)))))

(deftest list-device-names/empty-input
  (ok (null (cupertino::list-device-names (parse-devicectl-json "[]"))))
  (ok (null (cupertino::list-device-names nil))))
