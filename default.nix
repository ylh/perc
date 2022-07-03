# toplevel callPackage
{ lib, haskell, haskellPackages, removeReferencesTo }: let
  inherit (haskellPackages) callPackage pandoc-types file-embed;
  inherit (haskell.lib.compose) overrideCabal;
  addEmbed = { configureFlags ? [], buildDepends ? [], ... }: {
    configureFlags = configureFlags ++ [ "-fembed_data_files" ];
    buildDepends = buildDepends ++ [ file-embed ];
  };
  pandoc = overrideCabal addEmbed haskellPackages.pandoc;
  perc' = callPackage ./perc.nix {
    inherit pandoc;
  };
  addRemove =
    { pname, buildTools ? [], postInstall ? "", postFixup ? "", ... }: {
      buildTools = buildTools ++ [ removeReferencesTo ];
      postInstall = ''
        ${postInstall}
        remove-references-to \
          -t ${pandoc} \
          -t ${pandoc.data} \
          -t ${pandoc-types} \
          $out/bin/${pname}
      '';
      postFixup = ''
        ${postFixup}
        pushd $out
        ls | grep -xv bin | xargs rm -rf
        popd
      '';
    };
in overrideCabal addRemove perc'
