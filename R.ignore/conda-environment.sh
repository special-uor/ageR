conda create -n ageR
conda activate ageR
conda install -c r r-base
conda install -c r r-essentials
conda install -c r r-remotes
Rscript -e 'remotes::install_github("special-uor/ageR")'