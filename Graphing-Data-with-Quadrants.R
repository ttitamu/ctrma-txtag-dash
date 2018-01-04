# Graphing Data with Quadrants & Labels
# Michelle Plunkett
# January 4, 2018

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, installr, data.table, DT, ggrepel)

# Get the user's home directory path (Defining this value allows other users to easily run this script)
home <- path.expand("~")

# Determine if computer running this script is on windows, if not, don't change path to backslashes 
if (Sys.info()["sysname"]=="Windows") {
  home <- as.character(gsub("/","\\\\",home))
} else {
  home <- as.character(home)
}

# Set project directory
if (Sys.info()["sysname"]=="Windows") {
  projectdir <- "\\Projects\\Graph Making\\CTRMA Data\\"
  wd <- paste0(home,projectdir)
} else {
  projectdir <- "/Projects/Graph Making/CTRMA Data/"
  wd <- paste0(home,projectdir)
}
setwd(wd)

# Import the data file
datapts <- read.csv(paste0(wd,"ChrisNeedsHelp.csv"), stringsAsFactors = F)

# Rename columns
names(datapts) <- gsub("\\."," ",colnames(datapts))

# Get median values for x & y columns
xmed <- median(datapts$`Respondent Usability Score`)
ymed <- median(datapts$`Number of Respondents`)

# Graph the data with the "ggplot2" package & "ggrepel" for the labels
# Reference: https://stackoverflow.com/a/35510338
p <- ggplot(datapts, aes(label=`Function Code`, x=`Respondent Usability Score`, y=`Number of Respondents`)) +
     geom_point() +
     geom_text_repel() + 
     scale_x_continuous(limits=c(1,10), breaks=seq(1:10)) +
     scale_y_sqrt(minor_breaks=seq(0,1200,100),breaks=seq(0,1200,200),limits=c(0,1200), expand=c(0,0)) + 
     theme_minimal() +
     coord_cartesian() +
     geom_vline(xintercept = xmed) + 
     geom_hline(yintercept = ymed)

# Add quadrant numbers to plot
qcolor <- "blue" # define color here to easily change label color
p <- p + annotate("text",x=((10-xmed)/2),y=((1200-ymed)/2),label="1", color=qcolor, size=9) +
         annotate("text",x=8.25,y=((1200-ymed)/2),label="4", color=qcolor, size=9) +
         annotate("text",x=((10-xmed)/2),y=35,label="2",color=qcolor,size=9)+
         annotate("text",x=8.25,y=35,label="3",color=qcolor,size=9)

# View the plot
p

# Export the plot
fname <- paste0("Plot-v4-",qcolor) # define filename of exported plot
ftype <- ".png" # define desired file type
ggsave(paste0(fname,ftype), p, width=11, height=8.5, dpi=600)
embed_fonts(paste0(fname,ftype), outfile=paste0(fname,"-embed",ftype)) # Embeds font in PDF if font changed from default
