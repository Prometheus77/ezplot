# ezplot
Syntax simplification for ggplot2

## Installation

If installing for the first time, install via:

```r
devtools::install_github("Prometheus77/ezplot")
```

If that doesn't work, try this first:

```r
install.packages("devtools")
```

And then again try the previous command.

If re-installing to get latest update, use:

```r
devtools::install_github("Prometheus77/ezplot", force = TRUE)
```

Please report bugs, feature requests, questions, etc. in Issues.

## Basic syntax

Most of the work is done through the ezplot function. To see syntax for this function, type:

```r
?ezplot
```

## Shortcuts

Several shortcut functions exist so you don't always have to resort to ezplot(plot_type):

```r
ezline()
ezcol()
etc.
```

## Complex plots

Functions exist to create complex plots that are not straightforward to create simply by stacking ezplot objects.

### Waterfall plots

To create a waterfall plot, use:

```r
plot_waterfall()
```
