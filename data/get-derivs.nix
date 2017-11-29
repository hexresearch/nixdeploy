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
    , timers ? {}
    , ...
    }: derivations ++ lib.mapAttrsToList (name: fromService) services
                   ++ lib.mapAttrsToList (name: fromService) timers;

  perMachine = lib.mapAttrsToList (name: fromMachine) machines;
in lib.unique (lib.flatten perMachine)
