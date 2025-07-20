
# Run the functions 

Download the two files, in the *Sample_data* of this repo.

Open R studio.

Click on Session → Set Working Directory → Choose Directory

And then select the directory where you downloaded the files - metadata11.txt and microbiome11.csv

Load the library.


```r
library(mbX)
```

Run newer version of ezclean.

```r
ezclean(microbiome_data = "microbiome11.csv", metadata = "metadata11.txt", level = "g")
```
or, simply just pass the value for the parameters in the function. 

```r
ezclean("microbiome11.csv", "metadata11.txt", "g")
```
