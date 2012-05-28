(in-package :holly)

(defvar *quit-channel* (make-instance 'chanl:bounded-channel))

(defun start-server (&key (port 8082))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port))
  (make-processor)
  (start-stream-sender 'all "/tmp/holly.log"
		       :category-spec '(dribble+ x10)
		       :output-spec '(time category
				      context message))
  (entry-points)
  (trivial-timers:schedule-timer
   (trivial-timers:make-timer #'reschedule-timers)
   10
   :repeat-interval (* 60 30))) ;;reschedule every 30 minutes

(defun %start-server (args)
  (start-server :port (or (second args) 8081))
  (chanl:recv *quit-channel*))
