(in-package :holly)

(defun entry-points ()
  (push
   (hunchentoot:create-folder-dispatcher-and-handler "/s/" #P"/home/ryan/clbuild/source/holly/www/")
   hunchentoot:*dispatch-table*))

(defvar *tal-generator* (make-instance 'file-system-generator
		       :cachep nil
		       :root-directories (list #P"/home/ryan/clbuild/source/holly/templates/")))

(defun render-tal (tal-file &optional tal-env)
  (with-yaclml-output-to-string
    (funcall
     (load-tal *tal-generator* tal-file)
     tal-env
     *tal-generator*)))

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
		     'on-url (x10-device-href (x10-device-code dev) T)
		     'off-url (x10-device-href (x10-device-code dev) nil))))
     'status (when-let ((code-dir (hunchentoot:session-value 'x10-switch)))
	       (format nil "Set ~a to ~a" (car code-dir) (cadr code-dir)))
     'reschedule-url "/x10-reschedule"
     'timers (iter (for tm in *x10-timers*)
		   (collect (tal-env 'name
				     (timer:timer-name tm)))))))

(defun x10-device-href (code on-p)
  (format nil "/x10-switch?code=~a&dir=~a" code (if on-p "fon" "foff")))

(hunchentoot:define-easy-handler (x10-switch :uri "/x10-switch")
    (code dir)
  (change-x10-device code dir)
  (setf (hunchentoot:session-value 'x10-switch) (list code dir))
  (render-tal "x10-switch.tal"))

(hunchentoot:define-easy-handler (x10-reschedule :uri "/x10-reschedule")
    ()
  (reschedule-timers)
  (render-tal "x10-reschedule.tal"))

