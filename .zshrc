# Antigen.
source "$HOME/antigen/antigen.zsh"

# Use oh-my-zsh.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git
antigen bundle heroku
antigen bundle pip
antigen bundle lein
antigen bundle subnixr/minimal
antigen bundle command-not-found

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting

# Load the theme.
antigen theme subnixr/minimal

# Tell Antigen that you're done.
antigen apply

# Aliases.
#alias gotop='gotop -c monokai'
alias ls='exa'
alias cat='bat'
alias tmux-gui='tmux new-session \; new-window "tmux set-option -ga terminal-overrides \",$TERM:Tc\"; tmux detach"; tmux attach'
alias qs='fzf --preview "bat --style=numbers --color=always {} | head -500"'
alias d='kitty +kitten diff'
function fz() {
    if (( $# == 1 ))
    then find . -name "*.$1" | qs | xargs -I {} vim {}; fi
    qs | xargs -I {} vim {};
}

# Path to your oh-my-zsh installation.
#export ZSH="$HOME/.oh-my-zsh"
#source $ZSH/oh-my-zsh.sh

# Fix ranger.
alias ranger='ranger --choosedir=$HOME/.rangerdir; LASTDIR=`cat $HOME/.rangerdir`; cd "$LASTDIR"'

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/mccoybecker/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/mccoybecker/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/mccoybecker/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/mccoybecker/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

eval "$(starship init zsh)"
eval "$(zoxide init zsh)"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
