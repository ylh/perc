# haskellPackages.callPackage
{ lib, mkDerivation
, base, text, bytestring, containers
, pandoc, pandoc-types, blaze-html, data-default
}:
mkDerivation {
  pname = "perc";
  version = "1.0";
  src = ./.;
  
  libraryHaskellDepends = [
    base text bytestring containers
    pandoc pandoc-types blaze-html data-default
  ];
  isLibrary = false;
  
  license = lib.licenses.isc;
}
