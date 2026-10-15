#lang racket/base
;;; docs.rkt — the signatures in the installed Racket documentation, for
;;; Cadre's Racket docset
;;;
;;; Copyright (c) 2026 Jessie Hughart. MIT License (see LICENSE beside this file).
;;;
;;; `racket docs.rkt` prints one line per documented export, as Common Lisp
;;; data: (name (library ...) kind (signature-line ...) url). Kind is what
;;; the docs call it ("procedure", "syntax", "value", "parameter", ...), and
;;; the signature lines are the docs' blue boxes: the call and its contracts.
;;; Nothing here is copied from the documentation into Cadre: Cadre makes the
;;; docset on the user's machine, from the Racket they have installed.

(require setup/xref
         scribble/xref
         scribble/manual-struct
         scribble/blueboxes
         racket/string
         "cl-data.rkt")

(module+ main
  (define xref (load-collections-xref))
  (define cache (make-blueboxes-cache #t))
  (define out (current-output-port))
  (for ([e (xref-index xref)]
        #:when (exported-index-desc? (entry-desc e)))
    (define desc (entry-desc e))
    (define tag (entry-tag e))
    (define lines
      (map (λ (l) (string-replace l "\u00A0" " "))
           (or (with-handlers ([exn:fail? (λ (_) #f)])
                 (fetch-blueboxes-strs tag #:blueboxes-cache cache))
               '())))
    (define url
      (with-handlers ([exn:fail? (λ (_) #f)])
        (define-values (path anchor) (xref-tag->path+anchor xref tag))
        (and path (format "file://~a~a" (path->string path) (if anchor (format "#~a" anchor) "")))))
    (write-cl (list (format "~a" (exported-index-desc-name desc))
                    (for/list ([lib (exported-index-desc-from-libs desc)]) (format "~s" lib))
                    (if (pair? lines) (car lines) "")
                    (if (pair? lines) (cdr lines) '())
                    url)
              out)
    (newline out)))
