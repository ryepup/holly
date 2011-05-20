(ql:quickload :holly)
(funcall (intern (symbol-name :start-server) (find-package :holly))
	 :port 8081)
