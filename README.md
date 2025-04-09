# **Plotzing: Customizable, Publication-Quality Plots in a Single Line of Code**

## **Introduction**
*NOTE: Plotzing is still in beta.*

Plotzing is an R package for generating publication-quality plots in a single line of code. Plotzing takes a different approach than other plotting packages by applying three guiding principles. First, the syntax required is simple; plots can be generated in one line of code, and nearly all commands use a common set of verbs (*set* and *show*), allowing users to type these verbs and see available commands without the need for memorization. Second, Plotzing plots  are maximally informative by default; for instance, barplots include bars, error bars, and individual datapoints by default (though these can be removed through additional commands). Finally, Plotzing integrates data cleaning directly into the data visualization process; for example, users can reshape their data, reverse-code variables, create dichotomized dummy variables, and perform other data cleaning tasks in the same line of code used for producing their graph. In this way, Plotzing dramatically reduces the barriers between acquiring data and viewing those data in a visual form. 

## **Installing the Plotzing Package**

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

When prompted to update other packages during the installation, press the enter key to skip this. Note that Plotzing only needs to be installed once, but it must be reloaded when reopening R using the command **`library(plotzing)`**. Keep in mind that, despite my capitalization of "Plotzing" throughout this document, loading plotzing into R requires a lowercase "p" (i.e., "plotzing"), as seen in the code above.

## **Updating the Plotzing Package**

To update Plotzing, simply unload the package if it's already loaded, reinstall the package, and reload Plotzing using the following code:

```{r, echo=TRUE, eval=FALSE}
#If Plotzing is currently loaded, unload it:
detach("package:plotzing", unload = TRUE) #Note the lowercase "p"

library(devtools) #Load devtools
install_github("plotzing/plotzing") #Install the package
library(plotzing) #Reload the package
```
## **Basic Plots**

To start graphing with Plotzing, begin by loading your dataframe into R. If you call your dataframe df, you do not need to specify **`data=`** when generating your graph, as Plotzing uses the dataframe df by default when no alternative dataframe is specified. 

After loading your data, you can use the functions *graph_line*, *graph_bar*, *graph_violin*, or *graph_scatterplot* to graph, as in the example below:

```{r, echo=TRUE, eval=FALSE}
#Load Plotzing
library(plotzing)

# Load your data
df <- read.csv("your_data_file.csv")

# Create a line graph
graph_line("y_variable", "x_variable", "color_variable", "panel_variable") #Quotation marks are needed around variable names
```

Replace **`"y_variable"`**, **`"x_variable"`**, **`"color_variable"`**, and **`"panel_variable"`** with the column names of those variables you wish to use, though make sure to keep quotes surrounding variable names. Note that the order of variables specified moves from left-to-right on the graph; the y-axis is specified first, followed by the x-axis, legend, and panels. If you don't wish to follow this order, you can specify your dependent variable, your independent variables, and your panel variable using the commands *dv=*, *iv1=*, *iv2=*, and *panelvariable=*. Simpler plots can be created (e.g., showing only an x and y-variable) by leaving out variables as desired. For instance, the following code would create a bar plot depicting "jealousy" on the y-axis, "condition" on the x-axis, and "gender" in the legend from a dataframe called df:

```{r, echo=TRUE, eval=FALSE}
graph_bar("jealousy","condition","gender") #Again, remember to keep quotation marks around variable names
```

*Specifying Custom Dataframes*

As mentioned previously, Plotzing will look for a datraframe called df in your global environment by default when no other dataframe is specified. However, if you wish to use another dataframe, you may specify this using **`data =`**.

## **Customizing Plots**

Nearly all commands in Plotzing can be accessed using the verbs *set* and *show*. Thus, typing these verbs in RStudio will bring up a list of available commands. For example, the code below generates a plot with the datapoints hidden (**`showdata=FALSE`**) and sets custom labels for the legend, title, and axes (using the commands **`setlegendtitle=`**, **`settitle=`**, etc.):

```{r, echo=TRUE, eval=FALSE}
graph_bar("y_variable", "x_variable", "color_variable", panelvariable="panel_variable", showdata=FALSE, settitle="Customized Bar Plot", setxaxistitle="X-Axis Title", setyaxistitle="Y-Axis Title", setlegendtitle="Legend Title")
```

You can also set the labels for each level of your x-variable, each level of your legend, and so on, as in the example below:

```{r, echo=TRUE, eval=FALSE}
graph_bar("y_variable", "x_variable", "color_variable", showdata=FALSE, setlegendtitle="Legend Title",setlegendlevels=c("legendlevel1","legendlevel2"),setxlevels=c("xaxislevel1","xaxislevel2")
```

To understand these commands more clearly, we can also generate a sample dataset (of jealousy responses to attractive or wealthy rivals) using the following code:

```{r, echo=TRUE, eval=FALSE}
jealousy_attractive<-c(2,7,6,1,4,5,6,7,5,4,5,4,4,4,2,2,1,3,3,5,6,5,6,7,7,2,3,5,4,7)
jealousy_wealthy<-c(3,3,6,5,4,5,5,4,2,3,4,2,3,4,1,4,3,4,5,4,2,4,3,2,1,5,6,3,2,1)
income<-c(45000, 25000, 75000, 95000, 20000, 35000, 60000, 30000,40000,55000,30000,80000,20000,50000,85000,70000,90000,45000,90000,75000,40000,25000,55000,75000,45000,20000,30000,40000,60000,50000)
gender<-c("Men", "Women", "Women", "Men", "Men", "Men", "Women", "Women","Men","Men","Men","Women","Women","Men","Men","Women","Women","Women","Men","Men","Women","Men","Women","Men","Women","Women","Women","Men","Women","Men")
relationship_status<-c("Partnered","Single","Single","Partnered","Partnered","Partnered","Single","Single","Partnered","Single","Single","Partnered","Partnered","Single","Single","Partnered","Partnered","Partnered","Single","Partnered","Partnered","Single","Single","Single","Partnered","Single","Partnered","Partnered","Single","Single")
df<-data.frame(jealousy_attractive,jealousy_wealthy,income,gender,relationship_status)
```

With this newly generated dataset, we can graph jealousy, gender and relationship status while also specifying custom titles, custom level labels, and so on in the code that follows:
 
```{r, echo=TRUE, eval=FALSE}
graph_bar("jealousy_wealthy", "gender", "relationship_status", setlegendtitle="Relationship Status",setlegendlevels=c("Single","Partnered"),setxlevels=c("Male","Female"),setxaxistitle="Gender",settitle="Jealousy as a Function fo Gender and Relationship Status",
```

*Improving graph aesthetics*

We can also improve the aesthetics of our graph. For example, the code below increases the thickness of our error bars, increases the size of our datapoints, and sets the colors of our graph to blue and turquoise: 

```{r, echo=TRUE, eval=FALSE}
graph_bar("jealousy_wealthy", "gender", "relationship_status", setdotsize=3, seterrorbarthickness=2, setcolors=c("blue","turquoise")
```

*Showing or hiding components of graphs*

Additional customizations are possible, some of which are dependent on the graph type. For example, when graphing violin plots, the commands **`showviolin=FALSE`** and **`showboxplot=FALSE`** may be used to hide the violins or hide the boxplots associated with these violins; when generating a scatterplot, the command **`showline=FALSE`** may be used to hide the regression line; and when generating bar and line plots, the command **`showerrorbars=FALSE`** may be used to hide error bars. Across all graph types, **`showdata=FALSE`** may be used to hide datapoints.

*Viewing other available commands*

Once again, because all commands use a common set of verbs ("set" and "show"), typing set or show within a graphing function will bring up a complete list of available commands in RStudio (e.g., typing graph_line(set  to see available set commands) 

## **Data Cleaning Within Plotzing**

Plotzing allows users to call certain data cleaning arguments when specifying their graph. 

*Reshaping data*

Suppose we want to reshape the data to create a graph depicting jealousy on the y-axis and rival type (attractive or wealthy) on the x-axis. To do this, we can simply specify multiple dependent variables surrounded by c(), as in the following example:

```{r, echo=TRUE, eval=FALSE}
# Reshaping data
graph_violin(c("jealousy_attractive", "jealousy_wealthy"), setxaxistitle="Rival Type", setyaxistitle="Jealousy", setxaxislevels=c("Attractive Rival", "Wealthy Rival"))
#The code above will change the dataset from wide-to-long and graph jealousy as a function of rival type.
```

*Median Splits (Dichotomized Dummy Variables)*

Other cleaning options are also available. For instance, we can add income as a color variable and perform a median split on this variable, dividing subjects into those with high and low incomes: 
```{r, echo=TRUE, eval=FALSE}
# Performing median splits on data
graph_violin(c("jealousy_attractive", "jealousy_wealthy"),"income", setxaxistitle="Rival Type", setyaxistitle="Jealousy", setxaxislevels=c("Attractive Rival", "Wealthy Rival"), splitgroup=TRUE)
```

*Reverse-coding variables*

We can also reverse-code our x-variable, y-variable, and so on using the commands **`setreversecodex=TRUE`**, **`setreversecodey=TRUE`**, etc.

## **Using Plotzing in Quarto and RMarkdown**

Because Plotzing may modify the specified dataframe (e.g., through reshaping), the updated dataframe is printed by default. This may cause problems when using Plotzing with Quarto or RMarkdown. To hide all output other than the graph itself, add the code **`showoutput=FALSE`**.

## **Generating Animated Plots (NOTE: In Early Beta; Not Available for Line Plots)**
 
To generate animated plots that can be saved as gifs, the gganimate and magick packages are also required. Begin by installing these using install.packages:

 ```{r, echo=TRUE, eval=FALSE}
install.packages("gganimate") #For generating animations
install.packages("magick") #For saving gif files
 ```

Once you have these packages installed, simply add the argument **`showanimation=TRUE`** and include two independent variables in your plotzing code. Doing so will change the second independent variable from a color variable to an animation variable. For example, the code below will generate an animated violin plot that transitions between single and partered subjects: 
 
 ```{r, echo=TRUE, eval=FALSE}
 # Animated violin plot
graph_violin("jealousy_attractive","gender","relationship_status",showanimation=TRUE,setyaxistitle="Jealousy",setxaxistitle="Gender") 
#Generate a violin plot which shows an animated transition between the two relationship_status levels (single and partnered)
 ```
![violinplotplotzingexample](https://github.com/user-attachments/assets/a18952e3-8a02-48ba-9d7a-6993a8fc5b36)

You can adjust the font size of the heading (showing "Single" or "Partnered" in this example) using **`setanimationheadingsize=`**. You can adjust the transition time between graphs using **`setanimationtransitionlength=`** or adjust the length of time that each graph is shown using **`setanimationlength=.`** (For example, **`setanimationlength=4`** would cause each graph to be shown for approximately 4 seconds.) Note that you may choose to set different speeds for each graph or each transition using **`c()`**. For instance, in an animation with 2 graphs, one could show the first graph only briefly and the second graph for 4.5 seconds by typing **`setanimationlength=c(0,4.5)`**. 

For repeated-measures data, you may also set an ID variable using the the command **`setanimationid=`** and specify the name of the ID variable in your dataframe (not currently available for bar plots). Setting an ID variable ensures that the movement of each individual datapoint correctly reflects the changes associated with each individual subject. You can adjust the size of your animation in pixels using the commands **`setanimationheight=`** and **`setanimationwidth`**= . 

**_Keep in mind, however, that animated graphs are still in the very early beta stages, and customization options are somewhat limited. Regression lines in scatterplots are not currently supported, animated bar plots cannot yet show individual datapoints or use setanimationid, and line plots cannot be animated. To save an animation, see the heading entitled "Saving Graphs and Animations" below._**

## **Generating Multiple Graphs Simultaneously**

For more advanced users, the *graph_master()* function may be used to generate multiple graphs simultaneously. For instance, the code below generates a bar graph with jealousy_wealthy on the y-axis and income (high or low) on the x-axis, and a violin plot with jealousy_attractive on the y-axis and income (high or low) again on the x-axis:

```{r, echo=TRUE, eval=FALSE}
graph_master(list("jealousy_attractive", "jealousy_wealthy"),"income",splitx=TRUE, graphtype=c("bar","violin"))
```

In the example above, the **`list()`** command is used to generate graphs with different dependent variables. However, the **`list()`** command may be used to generate multiple independent variables instead by specifying the IVs in a list instead (e.g., **`graph_master("jealousy_attractive",list("income","gender"),graphtype=c("scatterplot","line"))`**). Alternatively, when omitting the **`list`** argument, users may plot the same variables across multiple graph types, as in the example below:

```{r, echo=TRUE, eval=FALSE}
graph_master("jealousy_attractive","gender", "relationship_status",graphtype=c("bar","violin")) #Generate the same graph in the form of a bar plot and in the form of a violin plot
```
 
 ## **Saving Graphs and Animations**

**Saving Graphs**

Although users may use the point-and-click save functions in RStudio, the images saved through this method will be low in quality. To save higher quality images, users may use the **`save_graph()`** function. The **`save_graph()`** function saves the last graph generated by the user to their working directory as a high-quality (500 dpi) image. Users only need to specify the filename, and, if desired, the size of the graph in inches. For example, the following code saves the last generated graph to the working directory and specifies the size:

```{r, echo=TRUE, eval=FALSE}
# Saving graphs
save_graph("mygraph.png",height=6,width=9) #Saves the most recently generated plot to the working directory as a file called mygraph.png with a height of 6 inches and a width of 9 inches
```

Alternatively, users can have the height and width generated by default (e.g., **`save_graph("mygraph.png")`** would also work). 

**Saving Animations**

To save animated graphs as gifs, use the **`save_gif()`** function, with the filename.gif surrounded in quotes. For exampple: 

```{r, echo=TRUE, eval=FALSE}
# Saving gifs
save_gif("myanimation.gif") #Saves the most recently generated animation to the working directory as a file called myanimation.gif
```

Note that to adjust the height and width using **`setanimationheight`** and **`setanimationwidth`** you must do so when generating the animation--not when saving it (e.g., **`graph_violin("dependentvariable","independentvariable","animationvariable",setanimationheight=400,setanimationwidth=500`**).

## **Contact Me**

Plotzing was developed by Benjamin Gelbart, a PhD candidate at the University of California, Santa Barbara. If you're having trouble, notice a glitch, or want to offer feedback, feel free to email me at bgelbart[at]ucsb.edu. I would love to hear from you!
