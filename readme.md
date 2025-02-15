# "`singleRcapture`: An Package for Single-Source Capture-Recapture Models"

+ [the current version of the paper](singleRcapture/singleRcapture.pdf)
+ [Arxiv version](https://arxiv.org/abs/2411.11032)

## How to cite

Arxiv version: `Chlebicki, P., & Beręsewicz, M. (2025). singleRcapture: An R Package for Single-Source Capture-Recapture Models. arXiv preprint arXiv:2411.11032.`

```
@misc{chlebicki2025singlercapturerpackagesinglesource,
      title={singleRcapture: An R Package for Single-Source Capture-Recapture Models}, 
      author={Piotr Chlebicki and Maciej Beręsewicz},
      year={2025},
      eprint={2411.11032},
      archivePrefix={arXiv},
      primaryClass={stat.AP},
      url={https://arxiv.org/abs/2411.11032}, 
}
```

## Acknowledgements

The authors' work has been financed by the National Science Centre in Poland, OPUS 20, grant no. 2020/39/B/HS4/00941. 

## Code for reproduction of results

```{r}
file.copy("singleRcapture/singleRcapture.pdf", "submission/singleRcapture.pdf")
knitr::purl("singleRcapture/singleRcapture.Rmd", "submission/code-new.R", documentation = 0)
file.append("submission/code-new.R", "submission/custom-family.R") ## comment out dev.off()
knitr::spin("submission/code-new.R")
file.rename(paste0(c("submission/code-new.R", "code-new.md", "code-new.html")),
            paste0("submission/", c("code.R", "code.md", "code.html")))
unlink("figure", recursive = TRUE)

```

