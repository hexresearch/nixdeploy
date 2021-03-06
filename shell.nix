let
  pkgs = import ./pkgs.nix {};
  nixdeploy = import ./default.nix;
in pkgs.stdenv.mkDerivation {
  name = "nixdeploy";

  buildInputs = [
    nixdeploy
    pkgs.x11_ssh_askpass
  ];
}
