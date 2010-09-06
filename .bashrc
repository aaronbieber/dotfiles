#!/bin/bash

# Configure the command prompt.
if [[ $EUID == 0 ]]; then
	PS1='\[\033[01;31m\]\h \[\033[01;34m\]\W \$ \[\033[00m\]'
else
	PS1='\[\033[01;32m\]\u@\h \[\033[01;34m\]\W \$ \[\033[00m\]'
fi
export PS1

# add /usr/local/bin to the path
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/opt/local/bin:$PATH
export PATH=/opt/local/sbin:$PATH
export PATH=$HOME/bin:$HOME/bin/tools:$PATH
export PATH=/Library/Frameworks/Python.framework/Versions/3.1/bin:$PATH
export EDITOR=/usr/bin/vim
export NNTPSERVER=news.usenetserver.com
export TERM=xterm-color
export LSCOLORS=gxfxcxdxbxegedabagacad
export LC_ALL="C"

alias mysql=/usr/local/mysql/bin/mysql
alias mysqladmin=/usr/local/mysql/bin/mysqladmin

# Helpful functions
ll() { ls -G -lho "$@"; }
la() { ls -G -lhoa "$@"; }
proj() { vim --cmd 'vsplit | vertical resize 30' "$@"; }
sl() { screen -list; }
sr() { screen -x `findscreen`; }
sdr() { screen -dr `findscreen`; }
news() { slrn -C "$@"; }
top() {
	if [ $# -eq 0 ]; then
		/usr/bin/top -o cpu
	else
		/usr/bin/top "$@"
	fi
}
