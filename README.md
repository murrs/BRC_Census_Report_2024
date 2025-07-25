# BRC Census Report for 2024

This repository contains the materials for generating the Census report for 2024.

# setting up the software

## running on MacOS 15.5

Install Mac [Homebrew](https://brew.sh/).

```         
brew install r udunits gdal cmake ffmpeg rust librsvg
brew install --cask quarto

R
pkgs = c(
    "cartogram",
    "config",
    "cowplot",
    "data.table",
    "dplyr",
    "geomtextpath",
    "ggplot2",
    "kableExtra",
    "knitr",
    "scales",
    "sf",
    "survey",
    "systemfonts",
    "tmap",
    "tidytext",
    "tigris",
    "weights",
    "wordcloud2",
    "zipcodeR"
)
install.packages(pkgs, dependencies = TRUE)
q()

# define your path to the census data, e.g.:
cat >config.yml <<EOF
default:
  datadir: ~/_Data
EOF

quarto render
quarto preview
```
