deploy:
	rsync -av web/*.css web/*.html ciao.gmane.io:/home/httpd/gmane.io/stats/

update:
	$(HOME)/src/emacs/trunk/src/emacs -batch -l ~/.emacs -l ~/src/nst.el/nst.el -f nst
