{ ghc }:
with import <nixpkgs> { };

haskell.lib.buildStackProject {
  inherit ghc;
  name = "onajiyou-env";

  LANG = "C.UTF-8";
}
