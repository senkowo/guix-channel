hc:
	guix home container users/owo.scm -L ../

hr:
	guix home reconfigure users/owo.scm -L ../

sc:
	sudo -E guix system container systems/main.scm 

sr:
	sudo -E guix system reconfigure systems/main.scm
