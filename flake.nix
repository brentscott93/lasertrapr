{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
     pkgs =  (import nixpkgs {
	        inherit system;
		   config = {
		     permittedInsecurePackages = [
		       "hdf5-1.10.9"
		       ];
		    };
	    });
      rPkgs = with pkgs.rPackages; [config
                                    golem
                                    shiny
                                    processx
                                    attempt
                                    DT
                                    glue
                                    htmltools
                                    shinydashboard
                                    shinyWidgets
                                    shinyFiles
                                    dygraphs
                                    RcppRoll
                                    shinyjs
                                    pracma
                                    magrittr
                                    fresh
                                    changepoint
                                    gridExtra
                                    gt
                                    shinycssloaders
                                    hexbin
                                    depmixS4
                                    RColorBrewer
                                    rmarkdown
                                    data_table
                                    vroom
                                    survival
                                    survminer
                                    cowplot
                                    rstatix
                                    Hmisc
                                    plotrix
                                    broom
                                    drc
                                    truncdist
                                    ggstatsplot
                                    colourpicker
                                    minpack_lm
                                    reactlog
                                    testthat
                                    devtools
                                    lubridate
                                    tibble
                                    rhdf5];
    in {
      devShells.default = pkgs.mkShell {
        packages = [ pkgs.bashInteractive 
                     pkgs.R 
                     rPkgs
                     pkgs.pandoc];
      };
    });
}
