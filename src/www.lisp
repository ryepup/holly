(in-package #:holly)

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port 8080))
  (make-processor)
  (start-stream-sender 'all "/tmp/holly.log"
		       :category-spec '(dribble+ x10)
		       :output-spec '(time category
				      context message)))
(defun entry-points ()
  (push 
   (hunchentoot:create-folder-dispatcher-and-handler "/" "/home/ryan/clbuild/source/holly/www/")
   hunchentoot:*dispatch-table*))

(defstruct x10-device
  (name "" :type string :read-only T )
  (code "" :type string :read-only T )
  (state 0 :type bit))

(defvar *x10-devices* (list (make-x10-device :name "TV Lamp" :code "a1")
			    (make-x10-device :name "Bar Lights" :code "a3")))

(defvar *tal-generator* (make-instance 'file-system-generator
				       :root-directories (list *default-pathname-defaults*
							       (merge-pathnames "source/holly/templates/"))))

(hunchentoot:define-easy-handler (x10 :uri "/x10") ()
  (hunchentoot:start-session)
  (with-output-to-string (*yaclml-stream*)
    (funcall
     (load-tal *tal-generator* "x10.tal")
     (tal-env 'devices
	      (iter (for dev in *x10-devices*)
		    (collect
			(tal-env
			 'name (x10-device-name dev)
			 'class (format nil "state~a" (x10-device-state dev))
			 'on-url (x10-device-href (x10-device-code dev)
						  T)
			 'off-url (x10-device-href (x10-device-code dev)
						   nil))))
	      'status (alexandria:when-let ((code-dir (hunchentoot:session-value 'x10-switch)))
			(format nil "Set ~a to ~a" (car code-dir) (cadr code-dir))))
     *tal-generator*)))

(defun x10-device-href (code on-p)
  (format nil "/x10-switch?code=~a&dir=~a" code (if on-p "fon" "foff")))

(hunchentoot:define-easy-handler (x10-switch :uri "/x10-switch")
    (code dir)
  (chanl:send *x10-channel* (list code dir))
  (setf (hunchentoot:session-value 'x10-switch) (list code dir))
  (with-output-to-string (*yaclml-stream*)
    (funcall
     (load-tal *tal-generator* "x10-switch.tal")
     (tal-env )
     *tal-generator*)))

(defvar *x10-processor* nil "thread for serializing x10 req.")
(defvar *x10-channel* (make-instance 'chanl:bounded-channel :size 5)
  "channel to convey x10 commands, max queue length 5")

(defcategory x10)

(defun process-x10 (channel-msg)
  (destructuring-bind (code dir) channel-msg
    (assert (member dir (list "fon" "foff") :test #'string=)
	    () "~a must be fon or foff" dir)
    (assert (member code *x10-devices* :key #'x10-device-code :test #'string=)
	    () "~a must be in the list of devices: ~a" code *x10-devices*)
    (let ((cmd (format nil "heyu ~a ~a" dir code)))
      (log-for x10 cmd)
      (log-for x10
	       "~{ ~a ~}"
	       (multiple-value-list (trivial-shell:shell-command cmd)))
      (log-for x10
	       "~{ ~a ~}"
	       (multiple-value-list (trivial-shell:shell-command cmd)))
      (setf (x10-device-state (find code *x10-devices* :key #'x10-device-code
				    :test #'string=))
	    (if (string= dir "fon") 1 0)))))

(defun make-processor ()
  (setf *x10-processor*
	(chanl:pexec (:name "X10 Processor")
	  (iterate (for x = (chanl:recv *x10-channel*))
		   (until (eql x :shutdown))
		   (process-x10 x)))))
