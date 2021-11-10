#! /bin/bash

sudo apt update
sudo apt install -y gnome-tweaks emacs jq git zsh curl apt-transport-https ca-certificates

sudo snap install bw

sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

bw login

item=$(bw get item "ssh key")
echo $item | jq .notes -r > ~/.ssh/id_rsa
echo $item | jq '.fields[] | select(.name == "public").value' -r > .ssh/id_rsa.pub

mkdir -p ~/.kube
item=$(bw get item "kubeconfig.yaml")
printf "%s" $item | jq .notes -r > ~/.kube/config

chmod 700 ~/.ssh
chmod 644 ~/.ssh/id_rsa.pub

mkdir -p ~/utils
pushd ~/utils
git clone git@github.com:grantdhunter/dotfiles.git
popd
ln -s ~/utils/dotfiles/zshrc ~/.zshrc
ln -s ~/utils/dotfiles/emacs.d ~/.emacs.d


# setup kube
sudo curl -fsSLo /usr/share/keyrings/kubernetes-archive-keyring.gpg https://packages.cloud.google.com/apt/doc/apt-key.gpg
echo "deb [signed-by=/usr/share/keyrings/kubernetes-archive-keyring.gpg] https://apt.kubernetes.io/ kubernetes-xenial main" | sudo tee /etc/apt/sources.list.d/kubernetes.list

sudo apt-get update
sudo apt-get install -y kubectl

# setup python
sudo apt-get update
sudo apt-get install -y  make build-essential libssl-dev zlib1g-dev \
     libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm \
     libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev

curl https://pyenv.run | bash

# setup go
curl -L https://golang.org/dl/go1.17.3.linux-amd64.tar.gz > go.tar.gz
tar -C /usr/local -xzf go.tar.gz
rm go.tar.gz


# setup dev
mkdir -p ~/development
pushd ~/development
git clone git@github.com:grantdhunter/goat-cluster.git
git clone git@github.com:grantdhunter/ikhnaie.git
popd
