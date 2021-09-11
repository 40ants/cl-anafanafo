(defpackage #:anafanafo/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog))
(in-package anafanafo/changelog)


(defchangelog (:ignore-words ("JSOWN"))
  (0.2.0 2021-09-11
         "- Switched to JSOWN because Jonathan fails to parse floats.

            Jonathan issues error when parsing:

            ```lisp
            (jonathan:parse \"3.06\")
            ```
          - Moved to a new documentation building engine.")
  (0.1.0 2021-01-31
   "Initial version."))
