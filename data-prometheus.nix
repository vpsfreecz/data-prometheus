{ mkDerivation, attoparsec, base, bytestring, containers, hspec
, lens, raw-strings-qq, stdenv, wreq, zlib
}:
mkDerivation {
  pname = "data-prometheus";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers wreq
  ];
  executableHaskellDepends = [
    attoparsec base bytestring lens wreq zlib
  ];
  testHaskellDepends = [ base hspec raw-strings-qq ];
  homepage = "https://github.com/sorki/data-prometheus";
  description = "Prometheus metrics data types and parser";
  license = stdenv.lib.licenses.bsd3;
}
