(asdf:defsystem #:holly
  :depends-on (#:iterate #:hunchentoot #:trivial-shell #:chanl #:log5 #:yaclml)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "www")))))
