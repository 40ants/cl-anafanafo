(defpackage #:anafanafo/ci
  (:use #:cl)
  (:import-from #:40ants-ci/jobs/linter
                #:linter)
  (:import-from #:40ants-ci/jobs/docs
                #:build-docs)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package anafanafo/ci)


(defworkflow docs
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :cache t
  :jobs ((build-docs)))


(defworkflow ci
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((linter)))
