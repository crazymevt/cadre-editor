;;;; package.lisp — headless tests for cadre/core

(defpackage #:cadre-tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:c #:cadre)))

(in-package #:cadre-tests)

(define-test cadre-tests)
