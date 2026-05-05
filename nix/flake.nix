# -*- compile-command: "nix-channel --update; nix profile upgrade --all"; -*-
{
  description = "Felixs Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let pkgs = import nixpkgs {
    system = "x86_64-linux";
    config.allowUnfree = true;
  };
  in {
    packages.x86_64-linux.default = pkgs.buildEnv {
      name = "my-packages";
      paths = with pkgs; [
        gleam
        erlang
	rebar3
	inotify-tools

	pwntools

	vbindiff
      ];
    };
  };
}
