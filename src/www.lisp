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

(defvar *x10-devices* '(("TV Lamp" "a1")
			("Bar Lights" "a3")))
(defun x10-table (stream)
  (cl-who:with-html-output (stream)
    (:table :border 1 :padding 3 :spacing 5 :width "100%"
     (:tbody
      (iterate (for (name code) in *x10-devices*)
	       (collect
		   (cl-who:htm (:tr (:td (cl-who:str name))
				    (:td :align "center" (x10-device-link code T stream))
				    (:td :align "center" (x10-device-link code nil stream))))))))))

(defun x10-device-link (code on-p s)
  (cl-who:with-html-output (s)
    (:a :href (x10-device-href code on-p)
	(cl-who:str (if on-p "ON" "OFF")))))

(hunchentoot:define-easy-handler (x10 :uri "/x10") ()
  (hunchentoot:start-session)
  (cl-who:with-html-output-to-string (s)
    (:html
     (:body
      (:div (cl-who:str "Controls x10 devices"))
      (x10-table s)
      (alexandria:when-let ((code-dir (hunchentoot:session-value 'x10-switch)))
	(cl-who:htm (:div (cl-who:fmt "Set ~a to ~a" (car code-dir) (cadr code-dir)))))))))

(defun x10-device-href (code on-p)
  (format nil "/x10-switch?code=~a&dir=~a" code (if on-p "fon" "foff")))

(hunchentoot:define-easy-handler (x10-switch :uri "/x10-switch") (code dir)
  (chanl:send *x10-channel* (list code dir))
  (setf (hunchentoot:session-value 'x10-switch) (list code dir))
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head
      (:meta :http-equiv "refresh" :content "3;url=/x10"))
     (:body
      (:div
       (cl-who:fmt "Sent ~a ~a" dir code))))))


(defvar *x10-processor* nil "thread for serializing x10 req.")
(defvar *x10-channel* (make-instance 'chanl:bounded-channel :size 5)
  "channel to convey x10 commands, max queue length 5")

(defcategory x10)

(defun process-x10 (channel-msg)
  (destructuring-bind (code dir) channel-msg
    (assert (member dir (list "fon" "foff") :test #'string=)
	    () "~a must be fon or foff" dir)
    (assert (member code *x10-devices* :key #'cadr :test #'string=)
	    () "~a must be in the list of devices: ~a" code *x10-devices*)
    (let ((cmd (format nil "heyu ~a ~a" dir code)))
      (log-for x10 cmd)
      (log-for x10
	       "~{ ~a ~}"
	       (multiple-value-list (trivial-shell:shell-command cmd)))
      (log-for x10
	       "~{ ~a ~}"
	       (multiple-value-list (trivial-shell:shell-command cmd))))))

(defun make-processor ()
  (setf *x10-processor*
	(chanl:pexec (:name "X10 Processor")
	  (iterate (for x = (chanl:recv *x10-channel*))
		   (until (eql x :shutdown))
		   (process-x10 x)))))
