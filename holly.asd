(asdf:defsystem #:holly
  :depends-on (#:iterate #:hunchentoot #:trivial-shell #:chanl #:log5 #:yaclml
			 #:drakma #:cxml #:local-time #:trivial-timers #:cl-json)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "x10")
			     (:file "calendar")
			     (:file "www")
			     (:file "holly")))))
