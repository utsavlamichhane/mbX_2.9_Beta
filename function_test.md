
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
You will have the output into a dedicated directory in your working directory.

Run newer version of ezviz.

```r
ezviz(microbiome_data = "microbiome11.csv", metadata = "metadata11.txt", level = "g", selected_metadata = "BMIClass", top_taxa = 10, flip = "True")
```

or, simply just pass the value for the parameters in the function. 

```r
ezviz("microbiome11.csv", "metadata11.txt","g", "BMIClass", top_taxa = 10, flip = "True")
```

In our ezviz funtion explictly mentioning the parameter *top_taxa* or *threshold* is necessary because both parameter works on to filter the taxa by average abundance. 


You will have the outputs into a dedicated directory in your working directory.

Run the brand new ezstat function.


```r
ezstat(microbiome_data = "microbiome11.csv", metadata = "metadata11.txt", level = "g", selected_metadata = "BMIClass")
```

or, simply just pass the value for the parameters in the function. 

```r
ezstat("microbiome11.csv", "metadata11.txt", "g", "BMIClass")
```
