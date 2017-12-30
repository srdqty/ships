{ mkDerivation, base, binary, bytestring, hspec, stdenv }:
mkDerivation {
  pname = "ships";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base binary bytestring ];
  executableHaskellDepends = [ base binary bytestring ];
  testHaskellDepends = [ base binary bytestring hspec ];
  homepage = "https://github.com/srdqty/ships#readme";
  license = stdenv.lib.licenses.bsd3;
}
