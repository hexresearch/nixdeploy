# Uses ubuntu 16.04 LTS
set -x
nix-shell ../../shell.nix --command "nixdeploy deploy --argstr \"testMachineHost:$1\" --argstr \"testMachineUser:ubuntu\" ./deploy.nix --verbose"
