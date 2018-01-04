{ mkDerivation, base, binary, bytestring, hspec
, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "ships";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary bytestring optparse-applicative
  ];
  executableHaskellDepends = [
    base binary bytestring optparse-applicative
  ];
  testHaskellDepends = [
    base binary bytestring hspec optparse-applicative
  ];
  homepage = "https://github.com/srdqty/ships#readme";
  license = stdenv.lib.licenses.bsd3;
}
