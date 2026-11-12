deploy:
	rsync -av web/*.css web/*.html ciao.gmane.io:/home/httpd/gmane.io/stats/
