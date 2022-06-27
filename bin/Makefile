.PHONY: decrypt

all:
	@echo "Please run a specific recipe."

encrypt:
	gpg --output freedns.token.txt.gpg --symmetric ~/.freedns.token.txt

# "52"
decrypt:
	gpg --output ~/.freedns.token.txt --decrypt freedns.token.txt.gpg
