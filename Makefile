hc:
	guix home container users/owo.scm -L ../

hr:
	guix home reconfigure users/owo.scm -L ../

sc:
	sudo -E guix system container systems/main.scm 

sr:
	sudo -E guix system reconfigure systems/main.scm

authenticate:
	die(){ echo "ERROR"; exit 1 }
	guix git authenticate commit senko || die
	[ $(git config --get user.signingkey) ] || die

# make magit run file before git push?
