# Companion Materials

This folder contains the companion materials for our paper *"HPC on a budget: on the CPU's impact on dense linear algebra computation with GPUs"*, submitted to
[SSCAD-WIC 2025](https://sscad2025.ufms.br/?page_id=277). Materials include scripts and data used to 
generate the results presented. It contains the following structure:

```
├── outputs/ # Containing the original output files used to write the paper
│   ├── xeon_performance   # Containing Xeon-related outputs
│   ├── i9_performance     # Containing i9-related outputs
│   └── parquets           # Containing execution trace data from the i9 CPU
└── plot_scripts/ # Containing the scripts used to generate the plots present on the paper
└── scripts/ # Containing the slurm files so anyone can replicate the experiments and extract their own outputs
``` 

## Figures
The figures can be created by running the R script files from the plot_scripts folder. Their execution depends on the ggplot2, dplyr, RColorBrewer, arrow and stringr packages,
which can be installed via:

```
install.packages("<package_name>")
```

## Slurm scripts
The scripts were written for the PCAD environment, so they need to be adjusted to be run on different setups. They also require [StarPU](https://starpu.gitlabpages.inria.fr/) and [Chameleon](https://solverstack.gitlabpages.inria.fr/chameleon/), which can be installed via [Spack](https://spack.io/). Our experiments had StarPU on commit 146ce9d8 and Chameleon on commit 3e958439.