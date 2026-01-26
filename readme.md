# "`singleRcapture`: An Package for Single-Source Capture-Recapture Models"

+ [Published version](https://doi.org/10.18637/jss.v115.i01)
+ [Arxiv version](https://arxiv.org/abs/2411.11032)

## How to cite

Chlebicki, P., & Berȩsewicz, M. (2025). singleRcapture: An R package for single-source capture-recapture models. Journal of Statistical Software, 115(1), 1–30. https://doi.org/10.18637/jss.v115.i01

```
@Article{,
    title = {{singleRcapture}: An {R} Package for Single-Source Capture-Recapture Models},
    author = {Piotr Chlebicki and Maciej Ber\k{e}sewicz},
    journal = {Journal of Statistical Software},
    year = {2025},
    volume = {115},
    number = {1},
    pages = {1--30},
    doi = {10.18637/jss.v115.i01},
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

