;;;; package.lisp

(defpackage :kbdd
  (:use #:cl #:stumpwm)
  (:export #:*locales*
           #:init
           #:subscribe
           #:kbdd
           #:get-current-layout
           #:set-layout
           #:set-layout-id
           #:next-layout
           #:prev-layout))
