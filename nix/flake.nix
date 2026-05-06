# -*- compile-command: "nix-channel --update; nix profile upgrade --all"; -*-
{
  description = "Felixs Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    pwndbg.url = "github:pwndbg/pwndbg";
  };

  outputs = { self, nixpkgs, pwndbg, ... }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    pwndbgPkg = pwndbg.packages.${system}.default;
  in {
    packages.x86_64-linux.default = pkgs.buildEnv {
      name = "my-packages";
      paths = with pkgs; [
        gleam
        erlang
	rebar3
	inotify-tools

	vagrant

	pwntools
	python314Packages.ropper

	vbindiff
      ] ++
      [pwndbgPkg];
    };
  };
}
