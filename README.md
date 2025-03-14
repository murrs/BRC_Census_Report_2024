# BRC Census Report for 2024

This repository contains the materials for generating the Census report for 2024.

# setting up the software

## running on MacOS 15.3

Install Mac [Homebrew](https://brew.sh/).

```
brew install quadro r udunits gdal

mkdir ~/.R
cat > ~/.R/Makevars <<EOF
OBJCXX = clang++ -std=c++11
EOF

R
pkgs = c(
	"cartogram", 
        "config",
	"ggplot2", 
	"kableExtra", 
	"survey", 
	"systemfonts", 
	"tigris", 
	"tmap", 
	"weights", 
        "zipcodeR",
)
install.packages(pkgs, dependencies = TRUE)
q()

# put the path to your data dir here:
cat >config.yml <<EOF
default:
  datadir: /Users/matto/Documents/census_data/
EOF

quarto render
quarto preview
```
