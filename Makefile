
holly:
	buildapp --load ~/quicklisp/setup.lisp \
	  --eval "(ql:quickload :holly)" \
	  --output www.holly --entry "holly:%start-server"

default: holly

install: 
	cp holly.conf /etc/init/holly.conf
