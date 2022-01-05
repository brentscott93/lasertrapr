FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libgmp-dev libicu-dev libmpfr-dev libpng-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.11")'
RUN Rscript -e 'remotes::install_version("vroom",upgrade="never", version = "1.5.7")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-2")'
RUN Rscript -e 'remotes::install_version("broom",upgrade="never", version = "0.7.10")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.2")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("gridExtra",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("rstatix",upgrade="never", version = "0.7.0")'
RUN Rscript -e 'remotes::install_version("cowplot",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("plotrix",upgrade="never", version = "3.8-2")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("colourpicker",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("ggstatsplot",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("truncdist",upgrade="never", version = "1.0-2")'
RUN Rscript -e 'remotes::install_version("drc",upgrade="never", version = "3.0-1")'
RUN Rscript -e 'remotes::install_version("Hmisc",upgrade="never", version = "4.6-0")'
RUN Rscript -e 'remotes::install_version("survminer",upgrade="never", version = "0.4.9")'
RUN Rscript -e 'remotes::install_version("depmixS4",upgrade="never", version = "1.5-0")'
RUN Rscript -e 'remotes::install_version("hexbin",upgrade="never", version = "1.28.2")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("gt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("changepoint",upgrade="never", version = "2.2.2")'
RUN Rscript -e 'remotes::install_version("fresh",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("pracma",upgrade="never", version = "2.3.3")'
RUN Rscript -e 'remotes::install_version("RcppRoll",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("dygraphs",upgrade="never", version = "1.1.1.6")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.2")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.20")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');lasertrapr::run_app()"
