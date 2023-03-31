# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="zhann"

plugins=(git)

source $ZSH/oh-my-zsh.sh

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
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:/usr/local/kotlinc/bin
export PATH=$PATH:/usr/local/gradle-7.5.1/bin
export PATH=$PATH:$HOME/.krew/bin
export PATH=$PATH:$HOME/.linkerd2/bin
export PATH=$PATH:/usr/local/swift/bin
export PATH=$PATH:$HOME/.cargo/bin
source <(kubectl completion zsh)

# python setup
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH=$PATH:$PYENV_ROOT/bin
eval "$(pyenv init -)"
export PATH=$PATH:$PYENV_ROOT/versions

# node setup
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# android setup
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
export ANDROID_HOME=$HOME/utils/android/sdk
export PATH=$PATH:$JAVA_HOME/bin:$ANDROID_HOME/cmdline-tools/latest/bin:$ANDROID_HOME/platform-tools:$ANDROID_HOME/emulator:$FLUTTER_HOME/bin



export LOCAL_CONFIG=$HOME/.local-config.sh
[ -f $LOCAL_CONFIG ] && source $LOCAL_CONFIG
