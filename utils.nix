{ config, lib, pkgs, ... }:

{
  emacsPkg = { code, config ? "", description, name, packages ? [], require ? false }:
    let pkg = pkgs.emacsPackages.trivialBuild {
      pname = name;
      version = "1.0.0";
      src = pkgs.writeText "${name}.el" ''
;;; ${name}.el --- ${description} -*- lexical-binding: t -*-

${code}

(provide '${name})'';
        };
      in rec {
        extraPackages = epkgs: packages ++ [ pkg ];
        extraConfig = ''
${if require then "(require '${pkg.pname})" else ""}
${config}
'';
    };
}
