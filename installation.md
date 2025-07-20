# Installing the package from your local system

Download the package file from the link below.

```
https://outlookuga-my.sharepoint.com/:u:/g/personal/ul54354_uga_edu/ERwbyZVbaXxPiZj1lIzSBLcBRk8givavpU2EEQolhHhyXA?e=x2aMKd
```
If you have trouble downloading the package file. 

Open R studio.

Click on *Session* → *Set Working Directory* → *Choose Directory*

And then select the directory where you downloaded the package file. 

Run the following code in your R console

```r
install.packages("mbX_0.2.0.tar.gz", repos = NULL, type = "source")
```
Load the library.

```r
library(mbX)
```
# Note on stable version:

Once we complete the initial testing phase here at the University, We will update the stable version of mbX_2 via CRAN.
