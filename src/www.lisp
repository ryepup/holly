(in-package :holly)

(defun resource-path (dirname)
  (truename
   (merge-pathnames dirname
		    (asdf:system-source-directory :holly))))

(defun entry-points ()
  (push (hunchentoot:create-folder-dispatcher-and-handler
	 "/assets/" (resource-path "www"))
	hunchentoot:*dispatch-table*))

(defvar *tal-generator* (make-instance 'file-system-generator
		       :cachep nil
		       :root-directories (list (resource-path "templates"))))

(defun render-tal (tal-file &optional tal-env)
;;  (push (cons 'utime (get-universal-time)) tal-env)
  (push (list (cons 'utime (format nil "~,1F"
			     (/ (get-universal-time) (* 24 3600.0))
			     )))
	
	tal-env)
  (concatenate 'string
	       "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">
"
	       (with-yaclml-output-to-string
		 (funcall
		  (load-tal *tal-generator* tal-file)
		  tal-env
		  *tal-generator*))))

(hunchentoot:define-easy-handler (x10 :uri "/x10") ()
  (hunchentoot:start-session)
  (render-tal "x10.tal"
    (tal-env
     'devices (iter
		(for dev in *x10-devices*)
		(collect
		    (tal-env
		     'name (x10-device-name dev)
		     'class (x10-device-state dev)
		     'next-time
		     (when-let ((ts (first (last (find (x10-device-name dev) *x10-timers*
					     :key #'(lambda (item) (x10-device-name (first item)))
					     :test #'string=)))))
		       (local-time:to-rfc1123-timestring ts))
		     
		     'code (x10-device-code dev))))
     'status (when-let ((code-dir (hunchentoot:session-value 'x10-switch)))
	       (format nil "Set ~a to ~a" (car code-dir) (cadr code-dir)))
     'reschedule-url "/x10-reschedule")))

(defun x10-device-href (code on-p)
  (format nil "/x10-switch?code=~a&dir=~a" code (if on-p "fon" "foff")))

(hunchentoot:define-easy-handler (x10-switch :uri "/x10-switch")
    (code dir)
  (change-x10-device code dir)
  (setf (hunchentoot:session-value 'x10-switch) (list code dir))
  (render-tal "x10-switch.tal"))

(hunchentoot:define-easy-handler (api-x10 :uri "/api/x10")
    (code dir)
  (change-x10-device code dir)
  "OK")

(hunchentoot:define-easy-handler (x10-reschedule :uri "/x10-reschedule")
    ()
  (reschedule-timers)
  (render-tal "x10-reschedule.tal"))

(hunchentoot:define-easy-handler (home :uri "/")
    ()
  (render-tal "home.tal"))

