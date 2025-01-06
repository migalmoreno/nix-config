{
  config,
  lib,
  pkgs,
  ...
}:

let
  cgit = pkgs.cgit.overrideAttrs (_: rec {
    version = "4dee601bb6042cf7db2472b9c34530850345f680";
    src = pkgs.fetchurl {
      url = "https://git.zx2c4.com/cgit/snapshot/cgit-${version}.tar.xz";
      sha256 = "WhpepqFhBYCBUrjO/OvcOgNZGcle2Xo2FviDN9A+eW0=";
    };
    gitSrc = pkgs.fetchurl {
      url = "mirror://kernel/software/scm/git/git-2.39.2.tar.xz";
      sha256 = "R1918Tc7LNTkOHBhhRdZZtXBH2jE2x5IwmJXxD3c8tY=";
    };
    patches = [
      (pkgs.fetchpatch {
        urls = [ "https://git.zx2c4.com/cgit/patch/?id=8ed1bef90f631989c0cadc326a163b874a64e02d" ];
        sha256 = "TrUJ+ppvKS75Wm2AjXKXyEp6KHWtHLE5mkt2IYAP0go=";
      })
    ];
  });
  cgit-org2html = pkgs.stdenv.mkDerivation {
    name = "cgit-org2html";
    src = pkgs.fetchFromGitHub {
      owner = "amartos";
      repo = "cgit-org2html";
      rev = "e34765572aeb328cc5d9bac08b9df2b34b39da46";
      sha256 = "1s2yxs8pvb3gjwgd3nxm4gq54lmwrir9fb73sd595b9nyrwk4zaw";
    };
    postPatch = ''
      substituteInPlace org2html --replace 'emacs' '${pkgs.emacs-nox}/bin/emacs'
    '';
    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/share
      cp org2html $out/bin
      cp css/org2html.css $out/share
      chmod +x $out/bin/org2html
      wrapProgram "$out/bin/org2html" \
        --set ORG2HTML_CSS_PATH "$out/share/org2html.css" \
        --set CGIT_CONFIG "/usr/share/cgit"
    '';
    buildInputs = [ pkgs.makeWrapper ];
  };
  package = config.services.cgit."git.migalmoreno.com".package;
  about-formatting = pkgs.writeShellScriptBin "about-formatting" ''
    cd "${package}/lib/cgit/filters/html-converters"
    if [ ! -d /var/cache/cgit/org2html ]; then
      mkdir -p /var/cache/cgit/org2html
    fi
    case "$(printf '%s' "$1" | tr '[:upper:]' '[:lower:]')" in
      *org|*) ${cgit-org2html}/bin/org2html; ;;
    	*.markdown|*.mdown|*.md|*.mkd) exec ./md2html; ;;
    	*.rst) exec ./rst2html; ;;
    	*.[1-9]) exec ./man2html; ;;
    	*.htm|*.html) exec cat; ;;
    	*.txt) exec ./txt2html; ;;
    esac
  '';
in
{
  services.gitolite = {
    enable = true;
    user = "git";
    group = "git";
    adminPubkey = pkgs.secrets.personal.publicSshKey;
    extraGitoliteRc = ''
      $RC{UMASK} = 0027;
      $RC{GIT_CONFIG_KEYS} = "gitweb\..*";
    '';
  };
  services.cgit."git.migalmoreno.com" = {
    enable = true;
    package = cgit;
    scanPath = "/var/lib/gitolite/repositories";
    settings = {
      project-list = "/var/lib/gitolite/projects.list";
      head-include = "${pkgs.writeText "head.html" ''
        <meta name="viewport" content="width=device-width initial-scale=1.0"/>
      ''}";
      footer = "${pkgs.writeText "footer" ""}";
      section-from-path = true;
      repository-directory = "/var/lib/gitolite/repositories";
      repository-sort = "name";
      case-sensitive-sort = false;
      root-desc = "Miguel Ángel Moreno's Git repositories";
      enable-git-config = true;
      enable-index-links = false;
      enable-index-owner = false;
      enable-commit-graph = true;
      enable-log-filecount = true;
      enable-log-linecount = true;
      readme = ":README";
      remove-suffix = true;
      clone-url = "https://git.migalmoreno.com/$CGIT_REPO_URL";
      about-filter = "${about-formatting}/bin/about-formatting";
      source-filter = "${package}/lib/cgit/filters/syntax-highlighting.py";
    };
    extraConfig = ''
      readme=:README.md
      readme=:README.org
    '';
  };
  services.nginx.virtualHosts."git.migalmoreno.com".locations = {
    "= /cgit.css" = {
      extraConfig = lib.mkForce ''
        alias ${pkgs.writeText "cgit.css" ''
          ${builtins.readFile "${pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/MatejaMaric/responsive-cgit-css/master/cgit.css";
            sha256 = "07l53sik7c6r3xj0fxc4gl9aw8315qgl5hhyh570l89fj4vy7yhc";
          }}"}
          ${builtins.readFile "${package}/cgit/cgit.css"}

          pre:has(code.hljs){
            background: none !important;
            padding: 0 !important;
          }

          pre code.hljs {
            background: inherit !important;
            padding: 1em !important;
            display: block;
          }

          @media only all and (prefers-color-scheme: light), (prefers-color-scheme: no-preference) {
            ${builtins.readFile "${pkgs.fetchurl {
              url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/styles/stackoverflow-light.min.css";
              sha256 = "L+76i0kfkr5WR7T97FBEaxeeyvDBvEY6U6RdNSDXFQ0=";
            }}"}
          }

          @media only all and (prefers-color-scheme: dark) {
            ${builtins.readFile "${pkgs.fetchurl {
              url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/styles/stackoverflow-dark.min.css";
              sha256 = "SQJYhu5P1AX7OWSN5VkiR/6PNXHa1zFH1pU4aFwN4GM=";
            }}"}
          }
        ''};
      '';
    };
    "= /cgit.js" = {
      extraConfig = ''
        alias ${pkgs.writeText "cgit.js" ''
          ${builtins.readFile "${package}/cgit/cgit.js"}
          ${builtins.readFile "${pkgs.fetchurl {
            url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/highlight.min.js";
            sha256 = "Rx75rpDEB69ED83Ejt/utWIQazJnvRLZkHHBYvtS7TI=";
          }}"}
          ${builtins.readFile "${pkgs.fetchurl {
            url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/languages/lisp.min.js";
            sha256 = "ngx+LnaCPvChB7mYlb0anglQ3+BQbxQ4ZMNpa/1vUwM=";
          }}"}
          ${builtins.readFile "${pkgs.fetchurl {
            url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/languages/clojure.min.js";
            sha256 = "v9aItax3xstVlHf1L3yNmyrYr4wrm0b/7TwD/LV6bFU=";
          }}"}
          ${builtins.readFile "${pkgs.fetchurl {
            url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/languages/nix.min.js";
            sha256 = "PakQNH1D1sJgzZqiEUf0rLbLlFe61NFgathuzMx9I+U=";
          }}"}
          ${builtins.readFile "${pkgs.fetchurl {
            url = "https://unpkg.com/@highlightjs/cdn-assets@11.10.0/languages/nginx.min.js";
            sha256 = "nlOkEO0QWr6dhIgCfeB+IECzcLWuTEZ7CNrCAsJmk/w=";
          }}"}
          hljs.registerAliases(["elisp"], { languageName: "lisp" });
          hljs.registerAliases(["sh"], { languageName: "bash" });
          hljs.registerAliases(["conf-colon"], { languageName: "plaintext" });
          hljs.configure({ cssSelector: 'pre code[class*="lang"]' });
          hljs.highlightAll();
        ''};
      '';
    };
  };
  users.users.${config.services.cgit."git.migalmoreno.com".user} = {
    extraGroups = [ "git" ];
  };
}
