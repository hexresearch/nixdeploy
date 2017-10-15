# Uses ubuntu 16.04 LTS
nix-shell ../../default.nix --command "nixdeploy deploy --argstr \"machine1:$1\" --argstr \"sshUser:ubuntu\" ./deploy.nix"
