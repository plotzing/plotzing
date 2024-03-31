# **Plotzing: Customizable, Publication-Quality Plots in a Single Line of Code**

## **Introduction**

*NOTE: Plotzing is still in beta.*

Plotzing is an R package for generating publication-quality plots in a single line of code. Plotzing differs from other graphing in packages in several key ways. First, other plotting packages, in most cases, are syntactically complex--requiring many lines of code to produce each graph. Second, other packages commonly require each component of a plot to be specified separately (e.g., separately specifying datapoints; error bars; bars, lines, or other geoms; etc.). Finally (and, perhaps, most importantly), other plotting packages require data to have already been cleaned; data must already have been reshaped if appropriate, coded correctly, standardized, etc. 

Plotzing takes a different approach to plotting by applying three guiding principles. First, Plotzing syntax is simple; plots can be generated using a single line of code, and nearly all commands use a common set of verbs (*set* and *show*), allowing users to type these verbs and see commands available to users without the need for additional instruction. Second, Plotzing plots include all relevant components at the outset; for instance, barplots include bars, error bars, and individual datapoints by default (though these can be removed, if desired, through additional commands). Finally, Plotzing integrates data cleaning directly into the data visualization process; for example, users can reshape their data, reverse-code variables, and more in the same line of code used for producing their graph. In this way, Plotzing dramatically reduces the barriers between acquiring data and viewing those data in a visual form. 

Here, I begin by explaining how to install Plotzing. I then discuss the basics of graphing in Plotzing and discuss some commonly used features. I conclude with some advanced commands and additional information about how to offer feedback or get help regarding the Plotzing package.

## **Installing and Updating the Plotzing Package**

At present, Plotzing is only available on Github. Because of this, users must first install the devtools package, which allows Github packages to be installed in R. The following code installs devtools and installs and loads Plotzing:

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
#Load Plotzing
library(plotzing)

# Load your data
df <- read.csv("your_data_file.csv")

# Create a line graph
graph_line("y_variable", "x_variable", "color_variable", "panel_variable") #Quotation marks are needed around variable names
```

Replace **`"y_variable"`**, **`"x_variable"`**, **`"color_variable"`**, and **`"panel_variable"`** with the relevant column names of those variables you wish to use. Make sure to keep quotes surrounding the names of variables. Note that the order of variables specified moves from left-to-right on the graph (the y-axis is specified first, followed by the x-axis, the legend, and the panels). If you don't wish to follow this order, however, you can specify your dependent variable, your independent variables, and your panel variable using the commands dv=, iv1=, iv2=, and panelvariable=. Additionally, you can create simpler plots (e.g., just using an x and y-variable) by leaving out relevant variables as desired. For instance, the following code would create a bar plot depicting "jealousy" on the y-axis, "condition" on the x-axis, and "gender" in the legend (as a color variable) from a dataframe called df:

graph_bar("jealousy","condition","gender") #Again, remember to keep quotation marks around variable names

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


## **Data Cleaning Within Plotzing**

Plotzing allows users to call certain data cleaning arguments when specifying their graph. For example, suppose you have a dataset with three columns: jealousy_attractive, referring to jealousy in response to attractive rivals, jealousy_wealthy, referring to jealousy in response to financially prosperous rivals, and income, referring to one's own yearly income. We can make a sample dataset to represent this using the following code:

```{r, echo=TRUE, eval=FALSE}
jealousy_attractive<-c(5,5,6,7,4,5,6,7)
jealousy_wealthy<-c(3,3,6,5,4,5,5,4)
income<-c(40000, 25000, 75000, 95000, 20000, 35000, 60000, 30000)
gender<-c("male", "female", "female", "male", "male", "male", "female", "female")
relationship_status<-c("single", "single", "married", "married", "married", "single", "married", "single")

df<-data.frame(jealousy_attractive,jealousy_wealthy,income,gender,relationship_status)

```

*Reshaping data*
Suppose we want to reshape the data to create a graph depicting jealousy on the y-axis and rival type (attractive or wealthy) on the x-axis. To do this, we can simply specify multiple dependent variables surrounded by c(), as in the following example:

```{r, echo=TRUE, eval=FALSE}
# Reshaping data
graph_violin(c("jealousy_attractive", "jealousy_wealthy"), setxaxistitle="Rival Type", setyaxistitle="Jealousy", setxaxislevels=c("Attractive Rival", "Wealthy Rival"))
#The code above will change the dataset from wide-to-long.
```

*Median Splits*
Other cleaning options are also available. For instance, we can add income as a color variable and perform a median split on this variable, dividing subjects into those with high and low incomes: 
```{r, echo=TRUE, eval=FALSE}
# Performing median splits on data
graph_violin(c("jealousy_attractive", "jealousy_wealthy"),"income", setxaxistitle="Rival Type", setyaxistitle="Jealousy", setxaxislevels=c("Attractive Rival", "Wealthy Rival"), splitgroup=TRUE)
```

*Reverse-coding variables*
We can also reverse-code variables if desired. For example, the code below reverse-codes our x and y-variables and creates a scatterplot of the relationship between jealousy in response to attractive rivals and jealousy in response to wealthy rivals:

```{r, echo=TRUE, eval=FALSE}
# Reverse-coding data
graph_scatterplot("jealousy_attractive", "jealousy_wealthy", setreversecodex=TRUE,setreversecodey=TRUE)
```

## **Using Plotzing in Quarto and RMarkdown**

Because Plotzing allows changes to your dataframe, such as reshaping, the updated dataframe is printed by default. This may cause problems when attempting to use Plotzing with file formats such as Quarto or RMarkdown. To remedy this, use the command **'showoutput=FALSE'** to hide all output other than the graph itself.

## **Generating Multiple Graphs Simultaneously**

For more advanced users, the graph_master() function may be used with the list() command to generate multiple graphs simultaneously. For instance, the code below generates a bar graph with jealousy_wealthy on the y-axis and income (high or low) on the x-axis, and a violin plot with jealousy_attractive on the y-axis and income (high or low) again on the x-axis:

```{r, echo=TRUE, eval=FALSE}
# Using the graph_master() function
graph_master(list("jealousy_attractive", "jealousy_wealthy"),"income",splitx=TRUE, graphtype=c("bar","violin"))
```

In the example above, the list() command is used to generate graphs with different dependent variables. However, the list() command may also be used to generate multiple independent variables instead by specifying a single dv and then the IVs in a list. In the code below, the user generates a scatterplot with income on the x-axis and a violin plot with gender on the x-axis. In both plots, jealousy is on the y-axis, and relationship status is used as a color variable in the legend.

```{r, echo=TRUE, eval=FALSE}
# Using the graph_master() function
graph_master("jealousy_attractive",list("income","gender"), "relationship_status",graphtype=c("scatter","violin")) #Generate a scatterplot with income on the x-axis and a violin plot with gender on the x-axis. 
```

Alternatively, users may plot the same variables across multiple graph types to see which graph type looks the best, as in the example below:

```{r, echo=TRUE, eval=FALSE}
# Using the graph_master() function
graph_master("jealousy","gender", "relationship_status",graphtype=c("bar","violin")) #Generate a bar plot and a violin plot of the same graph 
```

## **Animated Graphs**

Violin plots and scatterplots may be animated using the argument showanimation=TRUE. The inclusion of this argument will change the second independent variable from a color variable to an animation variable. For example, the code below will generate an animated violin plot that transitions between single and partered subjects: 

```{r, echo=TRUE, eval=FALSE}
# Animated violin plot
graph_violin("jealousy","gender", "relationship_status",showanimation=TRUE #Generate a violin plot which shows an animated transition between the two relationship_status levels 
```

If you are using repeated-measures data, you may also specify an ID variable using the the command setanimationid= and specifying the name of the ID variable in your dataframe. This ensures that the movement of each individual datapoint correctly reflects the changes associated with each individual subject.

Note that animated graphs are still under development, and, at present, customization options are limited. 

## **Saving Graphs and Animations**

To save high-quality graphs, users may use ggsave() functions built into ggplot2 or use the save_graph() built into Plotzing.
The save_graph() function automatically saves the last graph or animation generated by the user to their working directory and sets the default dpi to 500 (for high-quality images). Users only need to specify the filename, and, if desired, the height and width of the graph in inches. For example, the following code saves the last generated graph to the user's working directory and sets the height to be 6 inches and the width to be 9 inches:

```{r, echo=TRUE, eval=FALSE}
# Saving graphs
save_graph("mygraph.png",height=6,width=9)
```

Alternatively, users can simply just save the graph and have the height and width generated by default, as in the example below:

```{r, echo=TRUE, eval=FALSE}
# Saving graphs
save_graph("mygraph.png")
```

Although users may also use the saving functions built into RStudio, the resulting files saved through this method will be lower in quality (as the save_graph() function automatically sets the DPI to 500).

Note that, to save animations, users may use the anim_sav() command built into gganimate or the save_animation() command built into Plotzing. However, this latter command is still under development and does not yet support height and width commands.

## **Contact Me**

Plotzing was developed by Benjamin Gelbart, a PhD candidate at the University of California, Santa Barbara. The project benefits greatly from the contributions of the R community and users who provide valuable feedback and suggestions. If you're having trouble, notice a glitch, or want to offer feedback, feel free to email me at bgelbartatucsb.edu. I would love to hear from you!

