# git completion
if [ -f /usr/local/etc/bash_completion.d/git-completion.bash ]; then
	. /usr/local/etc/bash_completion.d/git-completion.bash
fi

# rbenv setup
if [ -d $HOME/.rbenv/bin ]; then export PATH=$HOME/.rbenv/bin:$PATH; fi
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Environment variables
export PATH=/usr/local/bin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/opt/local/bin:$PATH
export PATH=/opt/local/sbin:$PATH
export PATH=$HOME/bin:$HOME/bin/tools:$PATH
export NNTPSERVER=news.usenetserver.com
export LESS=FRXSQ
export CLICOLOR_FORCE=1
export LSCOLORS=gxfxcxdxbxegedabagacad
export TERM=xterm-256color

# Aliases
alias mysql=/usr/local/mysql/bin/mysql
alias mysqladmin=/usr/local/mysql/bin/mysqladmin

# Functions
ll() { ls -G -lho "$@"; }
la() { ls -G -lhoa "$@"; }

function unrar() {
	if [ $# -eq 1 ]; then
		/opt/local/bin/unrar x "$1"
	else
		/opt/local/bin/unrar "$@"
	fi
}

function top() {
	if [ $# -eq 0 ]; then
		/usr/bin/top -o cpu
	else
		/usr/bin/top "$@"
	fi
}

# Test for the presence of the real ssh-copy-id. If it is not present, create
# our pseudo ssh-copy-id.
which ssh-copy-id > /dev/null
if [ $? -gt 0 ]; then
	function ssh-copy-id() {
		if [ $# -eq 0 ]; then
			echo "Usage: ssh-copy-id [user@]hostname"
			return 92
		else
			# Snagged from commandlinefu.com/commands/view/188
			cat ~/.ssh/id_rsa.pub | ssh $1 "(cat > tmp.pubkey; mkdir -p .ssh; touch .ssh/authorized_keys; sed -i.bak -e '/$(awk '{print $NF}' ~/.ssh/id_rsa.pub)/d' .ssh/authorized_keys;  cat tmp.pubkey >> .ssh/authorized_keys; rm tmp.pubkey)"
		fi
	}
fi

# BASH PROMPT STUFF
#  Customize BASH PS1 prompt to show current GIT repository and branch.
#  by Mike Stewart - http://MediaDoneRight.com

#  SETUP CONSTANTS
#  Bunch-o-predefined colors.  Makes reading code easier than escape sequences.
#  I don't remember where I found this.  o_O

# Reset
Color_Off="\[\033[0m\]"       # Text Reset

# Regular Colors
Black="\[\033[0;30m\]"        # Black
Red="\[\033[0;31m\]"          # Red
Green="\[\033[0;32m\]"        # Green
Yellow="\[\033[0;33m\]"       # Yellow
Blue="\[\033[0;34m\]"         # Blue
Purple="\[\033[0;35m\]"       # Purple
Cyan="\[\033[0;36m\]"         # Cyan
White="\[\033[0;37m\]"        # White

# Bold
BBlack="\[\033[1;30m\]"       # Black
BRed="\[\033[1;31m\]"         # Red
BGreen="\[\033[1;32m\]"       # Green
BYellow="\[\033[1;33m\]"      # Yellow
BBlue="\[\033[1;34m\]"        # Blue
BPurple="\[\033[1;35m\]"      # Purple
BCyan="\[\033[1;36m\]"        # Cyan
BWhite="\[\033[1;37m\]"       # White

# Underline
UBlack="\[\033[4;30m\]"       # Black
URed="\[\033[4;31m\]"         # Red
UGreen="\[\033[4;32m\]"       # Green
UYellow="\[\033[4;33m\]"      # Yellow
UBlue="\[\033[4;34m\]"        # Blue
UPurple="\[\033[4;35m\]"      # Purple
UCyan="\[\033[4;36m\]"        # Cyan
UWhite="\[\033[4;37m\]"       # White

# Background
On_Black="\[\033[40m\]"       # Black
On_Red="\[\033[41m\]"         # Red
On_Green="\[\033[42m\]"       # Green
On_Yellow="\[\033[43m\]"      # Yellow
On_Blue="\[\033[44m\]"        # Blue
On_Purple="\[\033[45m\]"      # Purple
On_Cyan="\[\033[46m\]"        # Cyan
On_White="\[\033[47m\]"       # White

# High Intensty
IBlack="\[\033[0;90m\]"       # Black
IRed="\[\033[0;91m\]"         # Red
IGreen="\[\033[0;92m\]"       # Green
IYellow="\[\033[0;93m\]"      # Yellow
IBlue="\[\033[0;94m\]"        # Blue
IPurple="\[\033[0;95m\]"      # Purple
ICyan="\[\033[0;96m\]"        # Cyan
IWhite="\[\033[0;97m\]"       # White

# Bold High Intensty
BIBlack="\[\033[1;90m\]"      # Black
BIRed="\[\033[1;91m\]"        # Red
BIGreen="\[\033[1;92m\]"      # Green
BIYellow="\[\033[1;93m\]"     # Yellow
BIBlue="\[\033[1;94m\]"       # Blue
BIPurple="\[\033[1;95m\]"     # Purple
BICyan="\[\033[1;96m\]"       # Cyan
BIWhite="\[\033[1;97m\]"      # White

# High Intensty backgrounds
On_IBlack="\[\033[0;100m\]"   # Black
On_IRed="\[\033[0;101m\]"     # Red
On_IGreen="\[\033[0;102m\]"   # Green
On_IYellow="\[\033[0;103m\]"  # Yellow
On_IBlue="\[\033[0;104m\]"    # Blue
On_IPurple="\[\033[10;95m\]"  # Purple
On_ICyan="\[\033[0;106m\]"    # Cyan
On_IWhite="\[\033[0;107m\]"   # White

# Various variables you might want for your PS1 prompt instead
Time12h="\T"
Time12a="\@"
#PathFull="\w"
PathShort="\W"
NewLine="\n"
Jobs="\j"

# This PS1 snippet was adopted from code for MAC/BSD I saw from: 
# http://allancraig.net/index.php?option=com_content&view=article&id=108:ps1-export-command-for-git&catid=45:general&Itemid=96
# I tweaked it to work on UBUNTU 11.04 & 11.10 plus made it mo' better

pwdtail () { #returns the last 2 fields of the working directory
	pwd|awk -F/ '{nlast = NF -1;print $nlast"/"$NF}'
}

function prompt_command() {
	if [ $? -ne 0 ]; then
		ERRPROMPT=' ${?}! '
	else
		ERRPROMPT=" "
	fi

	local GIT=""
	local PATHSHORT=`pwdtail`
	local LOAD=`uptime|awk '{min=NF-2;print $min}'`

	function git_status() {
		git_status_output=$(git status 2> /dev/null) || return

		branch_name() {
			sed -n 's/# On branch //p' <<< "$git_status_output"
		}

		number_of_commits() {
			local branch_prefix='# Your branch is '
			local branch_suffix='by [[:digit:]]+'
			if [[ "$git_status_output" =~ ${branch_prefix}"$1".*${branch_suffix} ]]
			then
				echo ${BASH_REMATCH[0]//[^0-9]/}
			else
				echo 0 && return 1
			fi
		}

		match_against_status() {
			local pattern="$1"
			[[ "$git_status_output" =~ ${pattern} ]]
		}

		working_dir_clean() {
			match_against_status '(working directory clean)'
		}

		local_changes() {
			local added='# Changes to be committed'
			local not_added='# Changes not staged for commit'
			match_against_status "($added|$not_added)"
		}

		untracked_files() {
			match_against_status '# Untracked files'
		}

		dashline() {
			eval printf '%.0s-' {1..$1}
		}

		ahead_arrow() {
			if commits_ahead=$(number_of_commits "ahead")
			then
				echo -e "$bold$(dashline $commits_ahead)$Color_Off> $commits_ahead ahead"
			fi
		}

		behind_arrow() {
			if commits_behind=$(number_of_commits "behind")
			then
				echo "$commits_behind behind <$bold$(dashline $commits_behind)$Color_Off"
			fi
		}

		branch_part() {
			local red="\033[31m"
			local green="\033[32m"
			local yellow="\033[33m"
			local branch_colour=""

			if untracked_files
			then
				branch_colour=$red
			elif local_changes
			then
				branch_colour=$yellow
			elif working_dir_clean
			then
				branch_colour=$green
			fi
			echo "$branch_colour$(branch_name)$Color_Off"
		}

		local behind_part=$(behind_arrow)
		local ahead_part=$(ahead_arrow)

		if [[ ! "$behind_part" && ! "$ahead_part" ]]
		then
			prompt="$(branch_part)"
		else
			prompt="$(branch_part) $behind_part|$ahead_part"
		fi

		echo -e '\n'$IBlue'[ '$prompt$IBlue' ]'$Color_Off
	}

	export PS1=$IBlue'['$White'\u'$IWhite'@'$White'\h'$IBlack' ('$LOAD') '$White$Time12h$IBlue']'$Red$ERRPROMPT$IBlue'\w'$Color_Off$(git_status)'\n\$ '
}
PROMPT_COMMAND=prompt_command
