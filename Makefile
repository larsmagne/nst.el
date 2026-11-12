update:
	$(HOME)/src/emacs/trunk/src/emacs -batch -l ~/.emacs -l ~/src/nst.el/nst.el -f nst
	rsync -av web/ /home/httpd/gmane.io/stats/
