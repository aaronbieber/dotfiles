#!/bin/bash

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# This is from OS X, I'm not sure why.
test -r /sw/bin/init.sh && . /sw/bin/init.sh
