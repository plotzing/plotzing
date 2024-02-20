# **Plotzing: Customizable, Publication-Quality Plots in a Single Line of Code**

## **Introduction**

Plotzing is an R package for generating publication-quality plots in a single line of code. Plotzing is different from other graphing in packages in several key ways. First, other plotting packages, in most cases, are syntactically complex--requiring many lines of code to produce each graph. Second, other packages commonly require each component of a plot to be specified separately (e.g., separately specifying datapoints; error bars; bars, lines, or other geoms; etc.). Finally (and, perhaps, most importantly), other plotting packages require data to have already been cleaned; data must already have been reshaped if appropriate, coded correctly, standardized, etc. 

Plotzing takes a different approach to plotting by by applying three guiding principles. First, Plotzing syntax is simple; plots can be generated using a single line of code, and nearly commands use a common set of verbs (*set* and *show*), allowing users to type these verbs and see commands available to users without the need for additional instruction. Second, Plotzing plots include all relevant components at the outset; for instance, barplots include bars, error bars, and individual datapoints by default (though these can be removed, if desired, through additional commands). Finally, Plotzing integrates data cleaning directly into the data visualization process; for example, users can reshape their data, reverse-code variables, and more in the same line of code used for producing their graph. In this way, Plotzing dramatically reduces the barriers between acquiring data and viewing those data in a visual form. 

I begin by explaining how to install Plotzing. I then discuss the basics of graphing in Plotzing and discuss some commonly used features. I conclude with some more advanced commands and additional information about how to offer feedback or get help regarding the Plotzing package.

## **Installing and Updating the Plotzing Package**

AT present, Plotzing is only available on Github. Because of this, users must first install the devtools package, which allows Github packages to be installed in R. The following code installs devtools and installs and loads Plotzing:

```{r, echo=TRUE, eval=FALSE}
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")

# Load devtools
library(devtools)

# Install Plotzing from GitHub
install_github("plotzing/plotzing") #If prompted to update other packages, just press enter

# Load Plotzing
library(plotzing) #Note the lowercase "p"
```

Plotzing only needs to be installed once. Afterward, you can load Plotzing whenever you open R using the command **`library(plotzing)`** If you are prompted to update other packages during the installation process, such as ggplot2, feel free to press enter to skip these updates. Additionally, it's important to note that, despite my capitalization of "Plotzing" throughout this document, loading plotzing into R requires a lowercase "p" (i.e., "plotzing"), as seen in the code above.

If you wish to update Plotzing, simply unload Plotzing if it's already loaded, reinstall the package, and reload Plotzing using the following code:

```{r, echo=TRUE, eval=FALSE}
#If Plotizng is currently loaded, unload it:
detach("package:plotzing", unload = TRUE) #Note the lowercase "p"
#Code from before
library(devtools) #Load devtools
install_github("plotzing/plotzing") #Install the package
library(plotzing) #Reload the package
```

## **Basic Plots**

To start graphing with Plotzing, begin by loading your dataframe into R. If you call your dataframe df, you do not need to specify data= when generating your graph, as Plotzing uses the dataframe df by default when no alternative dataframe is specified. 

Once you have loaded your data, you can use the functions graph_line, graph_bar, graph_violin, or graph_scatterplot to create your graph, as in the following example:

```{r, echo=TRUE, eval=FALSE}
# Load your data
df <- read.csv("your_data_file.csv")

# Create a line graph
graph_line("y_variable", "x_variable", "color_variable", "panel_variable")
```

Replace **`"y_variable"`**, **`"x_variable"`**, **`"color_variable"`**, and **`"panel_variable"`** with the relevant column names of those variables you wish to use. The order of variables specified moves from left-to-right on the graph (the y-axis is specified first, followed by the x-axis, the legend, and the panels). If you don't wish to follow this order, however, you can specify your dependent variable, your independent variables, and your panel variable using the commands dv=, iv1=, iv2=, and panelvariable=. Additionally, you can create simpler plots (e.g., just using an x and y-variable) by leaving out relevant variables as desired. 

## **Customizing Plots**

Nearly all commands in Plotzing can be accessed using the verbs *set* and *show*. Thus, typing these verbs in RStudio will bring up a list of available commands. For example, the code below generates a plot with the datapoints hidden and sets both a custom title and custom axis labels:

```{r, echo=TRUE, eval=FALSE}
# Customizing a bar plot
graph_bar("y_variable", "x_variable", "color_variable", panelvariable="panel_variable", showdata=FALSE, settitle="Customized Bar Plot", setxaxistitle="X-Axis Title", setyaxistitle="Y-Axis Title")
```

When showdata is set to the default value of TRUE, you can change the size of your datapoints, the transparency of your datapoints, and so on as desired. For example:

```{r, echo=TRUE, eval=FALSE}
# Customizing a line plot
graph_line("y_variable", "x_variable", "color_variable", panelvariable="panel_variable", setdotsize=3, setdottransparency=.1, settitle="Customized Bar Plot", setxaxistitle="X-Axis Title", setyaxistitle="Y-Axis Title")
```


## **Data Cleaning and Data Visualization**

Plotzing allows users to call certain data cleaning arguments when specifying their graph. For example, suppose you have a dataset with three columns: jealousy_attractive, referring to jealousy in response to attractive rivals, jealousy_wealthy, referring to jealousy in response to financially prosperous rivals, and income, referring to one's own yearly income. We can make a sample dataset to represent this using the following code:

```{r, echo=TRUE, eval=FALSE}
jealousy_attractive<-c(5,5,6,7,4,5,6,7)
jealousy_wealthy<-c(3,3,6,5,4,5,5,4)
income<-c(40000, 25000, 75000,95000,20000,35000,60000,30000)

df<-data.frame(jealousy_attractive,jealousy_wealthy,income)

```

Suppose we want to reshape the data to create a graph depicting jealousy on the y-axis and rival type (attractive or wealthy) on the x-axis. To do this, we can simply specify multiple dependent variables surrounded by c(), as in the following example:

```{r, echo=TRUE, eval=FALSE}
# Reshaping data
graph_violin(c("jealousy_attractive", "jealousy_wealthy"), setxaxistitle="Rival Type", setyaxistitle="Jealousy", setxaxislevels=c("Attractive Rival", "Wealthy Rival"))
#The code above will change the dataset from wide-to-long.
```

Other cleaning options are also available. For instance, we can add income as a color variable and perform a median split on this variable, dividing subjects into those with high and low incomes: 
```{r, echo=TRUE, eval=FALSE}
# Reshaping data
graph_violin(c("jealousy_attractive", "jealousy_wealthy"),"income", setxaxistitle="Rival Type", setyaxistitle="Jealousy", setxaxislevels=c("Attractive Rival", "Wealthy Rival"), splitgroup=TRUE)
#The code above will change the dataset from wide-to-long.
```

Other plot types and cleaning options are also available. For example, the code below reverse-scores our x and y-variables and creates a scatterplot of the relationship between jealousy in response to attractive rivals and jealousy in response to wealthy rivals:

```{r, echo=TRUE, eval=FALSE}
# Reshaping data
graph_scatterplot("jealousy_attractive", "jealousy_wealthy", setreversecodex=TRUE,setreversecodey=TRUE)
#The code above will change the dataset from wide-to-long.
```
## **Using Plotzing in Quarto and RMarkdown**

Because Plotzing allows changes to your dataframe, such as reshaping, the updated dataframe is printed by default. This may cause problems when attempting to use Plotzing with file formats such as Quarto or RMarkdown. To remedy this, use the command **'showoutput==FALSE'** to hide all output other than the graph itself.

## **Generating Multiple Graphs Simultaneously**

For more advanced users, the graph_master() function may be used with the list() command to generate multiple graphs simultaneously. For instance, the code below generates a bar graph with jealousy_wealthy on the y-axis and income (high or low) on the x-axis, and a violin plot with jealousy_attractive on the y-axis and income (high or low) again on the x-axis:

```{r, echo=TRUE, eval=FALSE}
# Using the graph_master() function
graph_master(list("jealousy_attractive", "jealousy_wealthy"),"income",splitx=TRUE, graphtype=c("bar","violin"))
```

## **Contact Me**

Plotzing was developed by Benjamin Gelbart, a PhD candidate at the University of California, Santa Barbara. The project benefits greatly from the contributions of the R community and users who provide valuable feedback and suggestions. If you're having trouble, notice a glitch, or want to offer feedback, feel free to email me at bgelbartatucsb.edu. I would love to hear from you!

