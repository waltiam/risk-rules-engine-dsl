(in-package :asdf-user)
(asdf:defsystem "hack-butt"
  :version "0.1.0"
  :author "waltiam"
  :license ""
  :depends-on ()
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "fib")))))