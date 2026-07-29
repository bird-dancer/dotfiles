# -*- compile-command: "nix flake update && nix profile upgrade --all"; -*-
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
  in {
    packages.x86_64-linux.default = pkgs.buildEnv {
      name = "my-packages";
      paths = with pkgs; [

	# erlang
        gleam
        beamPackages.erlang
	beamPackages.rebar3
	erlang-language-platform
	inotify-tools


	# binary
	pwntools
	python314Packages.ropper
	vbindiff
	one_gadget

	# other
	pgcli

	# web
	bun
	tailwindcss_4
	typescript-language-server

	# slop
	antigravity-ide
	antigravity-cli
	# claude-code
	# claude-code-router
	opencode

	# emacs
	emacsPackages.pdf-tools
	emacsPackages.ghostel


	# sway
	# swaysettings
	# swayfx
      ];
    };
  };
}
