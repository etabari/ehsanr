
# ehsanr
R package to have a bunch of useful functions


load this package using:

```R
library(devtools)
install_github(repo='ehsanr', username='etabari')
```


In case you don't have write access to the default libPath you could use the following, to install ehsanr in your last library path entry.

```R
library(devtools)
withr::with_libpaths(new =  .libPaths()[length(.libPaths())], 
						install_github(repo='ehsanr', username='etabari')
						)
```
