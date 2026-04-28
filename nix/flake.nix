{
  description = "Felixs Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
    packages.x86_64-linux.default = pkgs.buildEnv {
      name = "my-packages";
      paths = with pkgs; [
        gleam
        erlang
	pwntools
	vbindiff
      ];
    };
  };
}
