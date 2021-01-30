(defpackage #:anafanafo-docs/docs
  (:nicknames #:anafanafo-docs)
  (:use #:cl)
  (:import-from #:mgl-pax)
  (:import-from #:anafanafo/core
                #:@index)
  (:export
   #:build))
(in-package anafanafo-docs/docs)


(defun build (&optional (root-section-name '@index))
  (check-type root-section-name symbol)
  
  (let ((root-section (symbol-value root-section-name)))
    (mgl-pax:update-asdf-system-readmes root-section
                                        :anafanafo)
  
    (mgl-pax:update-asdf-system-html-docs
     root-section :anafanafo
     :target-dir "docs/build/"
     :pages `((:objects (,root-section)
               :source-uri-fn ,(pax:make-github-source-uri-fn
                                :anafanafo
                                "https://github.com/40ants/cl-anafanafo"))))))
