;;;; -*- Mode: Lisp -*-
;;;; Author:
;;;;     Yujian Zhang <yujian.zhang@gmail.com>
;;;; Description:
;;;;     Configuration for the kernel.
;;;; License:
;;;;     GNU General Public License v2
;;;;     http://www.gnu.org/licenses/gpl-2.0.html
;;;; Copyright (C) 2018 Yujian Zhang

(in-package :cc)

(defparameter *include-regression-test* nil
  "Whether to include regression tests in the kernel:
     - t: Include regression tests
     - nil: NOT include regression tests")

(defun regression-container (forms)
  "Include the FORMS if *include-regression-test* is T"
  (when *include-regression-test*
    forms))
