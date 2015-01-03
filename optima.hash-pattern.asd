;;;; optima.hash-pattern.asd
;;;;
;;;; Copyright (c) 2015 Thayne McCombs <astrothayne@gmail.com>

(asdf:defsystem #:optima.hash-pattern
  :description "Optima patterns for hash tables"
  :author "Thayne McCombs <astrothayne@gmail.com>"
  :license "LLGPL"
  :depends-on (#:optima
               #:alexandria)
  :components ((:file "optima.hash"))
