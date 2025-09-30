```markdown
# functionR

Small utilities to render ggplot objects into embedded PNG data URIs and create an HTML dropdown + preview suitable for Quarto / R Markdown (results='asis').

## Installation

Install from GitHub (after you push the repo):

```r
# install.packages("remotes") # if needed
remotes::install_github("Assassins-An/functionR")
```

## Example

```r
library(functionR)
library(ggplot2)

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + geom_boxplot()

plots <- list(BoneMarrow = p, Kidney = p2)
# put this in a chunk with {r, results='asis', echo=FALSE}
create_plot_dropdown(plots, width = 7, height = 4, units = "in", dpi = 300)
```
```
