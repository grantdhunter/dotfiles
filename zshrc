# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="zhann"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8



# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"


setopt noincappendhistory
setopt nosharehistory

export LSP_USE_PLISTS=true
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='emacs'
else
  export EDITOR='emacs'
fi

e() {
    emacsclient -a "" -qc -n  "$@" > /dev/null &; disown
}
alias ec="emacsclient -qc -n"

export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.pyenv/bin
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/.krew/bin
export PATH=$PATH:$HOME/.linkerd2/bin
export PATH=$PATH:/usr/local/swift/bin

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"
source <(kubectl completion zsh)

export AWS_DEFAULT_REGION=us-east-1

aws-docker-login() {
    echo 'Logging into Docker...'
    aws ecr get-login-password | docker login --username AWS --password-stdin 095258169967.dkr.ecr.us-east-1.amazonaws.com
}

aws-get-codeartifact() {
    echo 'Setting CODEARTIFACT_TOKEN...'
    export CODEARTIFACT_TOKEN=$(aws codeartifact get-authorization-token --domain drivewyze --domain-owner 095258169967 --query authorizationToken --output text)
    echo 'Done'
}

aws-login() {
    PROFILE="${1:-default}"
    echo "logging in for " $PROFILE
    # Modify the line below based on your needs
    # aws-azure-login sometimes will prompt for the password every time which is annoying
    # so this get-caller-identity is a way to test and only prompt for login when required
    IDENT=$(aws sts get-caller-identity --profile $PROFILE 2> /dev/null)
    if [ $? -ne 0 ]; then
        aws-azure-login --no-prompt --profile $PROFILE
    else
        echo "already logged in to aws as" $(echo $IDENT | jq -r .Arn)
    fi
}

start-dockerd ()(
    if [[ ! -f /var/run/docker.pid ]]; then
       nohup sudo -b dockerd  > /dev/null 2>&1
    fi
)


if [[ $(grep -i microsoft /proc/version) ]]; then
    echo "running in ms mode"

    start-dockerd
    export DISPLAY=$(cat /etc/resolv.conf | grep nameserver | awk '{print $2}'):0
    aws-login

    aws-docker-login > /dev/null
    aws-get-codeartifact > /dev/null

fi
