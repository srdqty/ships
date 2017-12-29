{ compiler ? "ghc802"
, image-name
, image-tag
}:

let
  pkgs = import ./nixpkgs-pinned.nix {};

  haskellPackages = pkgs.haskell.packages."${compiler}".override {
    overrides = new: old: {
      ships =
        pkgs.haskell.lib.justStaticExecutables
          (old.callPackage ../. { });
    };
  };
in
  pkgs.dockerTools.buildImage {
    name = image-name;
    tag = image-tag;

    contents = [
      haskellPackages.ships
    ];

    config = {
      Entrypoint = [
        "ships"
      ];

      Env = [
        "VAR1=var1"
        "VAR2=var2"
      ];
    };
  }
