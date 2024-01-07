authenticate:
	guix git authenticate $$(git rev-parse HEAD) "$$(gpg --fingerprint | sed -n '/^\s/s/\s*//p')"
	[ $$(git config --get user.signingkey) = $$(gpg --list-secret-keys --keyid-format=long | sed -nr 's/^sec.*\/(\S*).*$$/\1/p') ]
	echo -e "\nSUCCESSFUL\n"

# make magit run file before git push?
