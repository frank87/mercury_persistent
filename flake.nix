{
  description = "My qt application";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
    extras.url = "/home/frank/src/mercury/nixos/extras";
  };

  outputs = { self, nixpkgs, flake-utils, extras }: {
	defaultPackage.x86_64-linux = with import nixpkgs { system = "x86_64-linux"; }; 
			extras.build{ 
				src=self; 
				pname="test"; 
				mercuryLibs = with extras.packages.x86_64-linux; [ posix ]; 
				buildInputs = [ pkgs.gdbm ]; 
			};
  };
 
}

