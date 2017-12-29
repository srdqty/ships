{ owner ? "NixOS"
, repo ? "nixpkgs"
, rev ? "1bc288591ea4fe3159b7630dcd2b57733d80a2ff"
}:

let
  pkgs = import <nixpkgs> {};

  url="https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";

  file = builtins.fetchurl url;
  json = builtins.toFile "data.json" ''
    { "url": "${url}"
    , "rev": "${rev}"
    , "owner": "${owner}"
    , "repo": "${repo}"
    }
  '';

  project-root = builtins.toString ../.;
in


pkgs.stdenv.mkDerivation rec {
  name = "generate-nixpkgs-json";

  buildInputs = [
    pkgs.jq
  ];


  shellHook = ''
    set -eu
    sha256=$(sha256sum -b ${file} | awk -n '{printf $1}')
    jq .+="{\"sha256\":\"$sha256\"}" ${json} > ${project-root}/nix/nixpkgs.json
    exit
  '';
}
