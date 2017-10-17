# Uses ubuntu 16.04 LTS
nix-shell ../../shell.nix --command "nixdeploy deploy --argstr \"machine1:$1\" --argstr \"machine2:$2\" --argstr \"machine3:$3\" --argstr \"sshUser:ubuntu\""
