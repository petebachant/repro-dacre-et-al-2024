FROM rocker/r2u:24.04
RUN apt update -qq && apt upgrade -y
RUN Rscript -e 'install.packages(c("ggplot2", "ggstatsplot", "ggpmisc", "ggpubr", "tidyverse", "broom", "AICcmodavg", "readxl", "lme4", "Matrix", "lmerTest", "car", "reshape2", "viridis", "scales", "MASS", "emmeans", "multcompView", "dplyr"))'
