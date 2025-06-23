{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
      warn-dirty = false
    '';
    gc = {
      automatic = true;
      options = "--delete-older-than 30d";
    };
  };
  nixpkgs.config.allowUnfree = true;
}
