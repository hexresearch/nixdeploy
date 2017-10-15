# Extracts derivations list from deployment description
{
  machines,
  ...
}:
let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;

  fromService = {
    unit
  , ...
  }: unit;

  fromMachine = {
      derivations ? []
    , services ? {}
    , ...
    }: derivations ++ lib.mapAttrsToList (name: fromService) services;

  perMachine = lib.mapAttrsToList (name: fromMachine) machines;
in lib.unique (lib.flatten perMachine)
