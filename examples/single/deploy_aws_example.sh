# Uses ubuntu 16.04 LTS
nix-shell ../../default.nix --command "nixdeploy deploy --argstr \"testMachineHost:$1\" --argstr \"testMachineUser:ubuntu\" ./deploy.nix --verbose"
