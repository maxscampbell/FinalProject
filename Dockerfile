FROM rstudio/plumber

RUN apt-get update -qq && apt-get install -y libssl-dev libcurl4-gnutls-dev libpng-dev libpng-dev pandoc

RUN R -e "install.packages(c('plumber','tidyverse','tidymodels'))"

COPY api.R api.R

COPY diabetes.csv diabetes.csv

EXPOSE 8000

ENTRYPOINT ["R", "-e", \ 
  "pr <- plumber::plumb('api.R'); pr$run(host='0.0.0.0', port=8000)"]