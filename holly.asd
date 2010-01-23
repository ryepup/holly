(asdf:defsystem #:holly
  :depends-on (#:iterate #:hunchentoot #:cl-who #:trivial-shell #:chanl #:log5)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "www")))))
