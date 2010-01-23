(asdf:defsystem #:holly
  :depends-on (#:iterate #:hunchentoot #:cl-who #:trivial-shell #:chanl)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "www")))))
