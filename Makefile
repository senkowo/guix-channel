hc:
	guix home container users/owo.scm -L ../

hr:
	guix home reconfigure users/owo.scm -L ../

sc:
	sudo -E guix system container systems/main.scm 

sr:
	sudo -E guix system reconfigure systems/main.scm

authenticate:
	guix git authenticate $$(git rev-parse HEAD) "$$(gpg --fingerprint | sed -n '/^\s/s/\s*//p')"
	[ $$(git config --get user.signingkey) = $$(gpg --list-secret-keys --keyid-format=long | sed -nr 's/^sec.*\/(\S*).*$$/\1/p') ]
	echo -e "\nSUCCESSFUL\n"

# make magit run file before git push?
