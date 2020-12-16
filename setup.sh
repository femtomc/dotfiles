#!/bin/bash

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env
cargo install bat
cargo install exa
cargo install zoxide
cargo install starship
sudo apt install zsh
sudo apt install neovim
sudo apt install ranger
sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
mkdir antigen
curl -L git.io/antigen > antigen/antigen.zsh
zsh
wget https://julialang-s3.julialang.org/bin/linux/x64/1.5/julia-1.5.3-linux-x86_64.tar.gz
tar -xzf julia-1.5.3-linux-x86_64.tar.gz
mv julia-1.5.3-linux-x86_64.tar.gz julia
