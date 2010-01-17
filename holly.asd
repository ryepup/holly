(asdf:defsystem #:holly
  :depends-on (#:iterate)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "www")))))
