(defpackage #:anafanafo-test/core
  (:use #:cl)
  (:import-from #:anafanafo))
(in-package anafanafo-test/core)

;; TODO: Add Rove testsuite here

(multiple-value-bind (path size)
    (anafanafo::guess-best-filename "Verdana" "normal")
  (ok (string= (pathname-name path)
               "verdana-11px-normal"))
  (ok (= size 11)))
