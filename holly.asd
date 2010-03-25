(asdf:defsystem #:holly
  :depends-on (#:iterate #:hunchentoot #:trivial-shell #:chanl #:log5 #:yaclml
			 #:drakma #:cxml #:local-time #:timer)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "x10")
			     (:file "www")
			     (:file "calendar")))))
