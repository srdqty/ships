with import ./nixpkgs-pinned.nix {};

stdenv.mkDerivation rec {
  name = "generate-cabal-and-project-nix-file";

  project-root = builtins.toString ../.;

  buildInputs = [
    haskellPackages.hpack
    cabal2nix
  ];

  shellHook = ''
    echo -e "Running hpack to generate cabal file...\n"
    hpack ${project-root}

    echo -e "\nRunning cabal2nix to generate default.nix ...\n"
    cabal2nix . > ${project-root}/default.nix

    exit
  '';
}
