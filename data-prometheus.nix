{ mkDerivation, attoparsec, base, bytestring, containers, hspec
, raw-strings-qq, stdenv, transformers
}:
mkDerivation {
  pname = "data-prometheus";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers transformers
  ];
  testHaskellDepends = [
    attoparsec base containers hspec raw-strings-qq
  ];
  homepage = "https://github.com/vpsfreecz/data-prometheus";
  description = "Prometheus metrics data types and parser";
  license = stdenv.lib.licenses.bsd3;
}
