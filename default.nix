{ mkDerivation, base, binary, bytestring, gitrev, hspec
, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "ships";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base binary bytestring ];
  executableHaskellDepends = [
    base binary bytestring gitrev optparse-applicative
  ];
  testHaskellDepends = [ base binary bytestring hspec ];
  homepage = "https://github.com/srdqty/ships#readme";
  license = stdenv.lib.licenses.bsd3;
}
