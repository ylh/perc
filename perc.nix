# haskellPackages.callPackage
{ lib, mkDerivation, base, bytestring, text, pandoc, data-default }:
mkDerivation {
  pname = "perc";
  version = "1.0";
  src = ./.;
  
  libraryHaskellDepends = [ base bytestring text pandoc data-default ];
  isLibrary = false;
  
  license = lib.licenses.isc;
}
