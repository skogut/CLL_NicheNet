library(shiny)
library(ggplot2)
library(nationalparkcolors)
library(shinyWidgets)

 expression_data<- read.csv("/Users/skogut/Desktop/Frietze_Lab/FrietzeLab/CLL_NicheNet/expression_data_shiny_cpm.csv")
# 
# 
# ui <- fluidPage(
#   
#   # App title ----
#   titlePanel("CPM Normalized Expression in CLL or stroma"),
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout(
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       
#       # Input: Selector for variable to plot against mpg ----
#       selectInput("gene", "Gene:", 
#                   c(colnames(expression_data)[4:ncol(expression_data)]))
#       
#       # textInput("caption", "Caption", "Data Summary"),
#       # verbatimTextOutput("value")
#       
#       # # search bar -not quite
#       # searchInput(
#       #   inputId = "search", label = "Enter your text",
#       #   placeholder = "A placeholder",
#       #   btnSearch = icon("search"),
#       #   btnReset = icon("remove"),
#       #   width = "450px"
#       # ),
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel(
#       
#       # Output: Formatted text for caption ----
#       h3(textOutput("caption")),
#       
#       # Output: Plot of the requested variable against mpg ----
#       plotOutput("Plot")
#       
#     )
#   )
# )
# # ---------------------------------------------------  
# # Data pre-processing: this should be saved as 1 easy to call file *
# # load in and combine counts matrices for CLL and stroma samples
# # need to exist on the galaxy server
# #expression_data<- read.csv("shinyapp_Cambridge/expression_data_shiny.csv")
# 
# 
# # Define server logic to plot various variables against mpg ----
# server <- function(input, output) {
#   # Compute the formula text ----
#   # This is in a reactive expression since it is shared by the
#   # output$caption and output$mpgPlot functions
#   formulaText <- reactive({
#     paste("Expression of", input$gene)
#   })
#   
#   # Return the formula text for printing as a caption ----
#   output$caption <- renderText({
#     formulaText()
#   })
#   
#   # Generate a plot of the requested gene across sample groups ----
#   # and only exclude outliers if requested
#   output$Plot <- renderPlot({
#     colors<- nationalparkcolors::park_palette("SmokyMountains")
#     gene<- input$gene
 # for testing plot
 gene<- "A1BG"
    gene_counts<- data.frame(expression_data[,c(2:3, as.numeric(which(colnames(expression_data)== gene)))])
 colnames(gene_counts)[3]<- "counts"
#     #gene_counts$genotype = sapply(strsplit(as.character(gene_counts$names), "_"), function(x)x[1])
#     #gene_counts$treatment = sapply(strsplit(as.character(gene_counts$names), "_"), function(x)x[2])
#     
#     ggplot(gene_counts, aes(x= names, y= counts, fill= names)) + geom_boxplot() + geom_point(aes(color= cell, shape = cell),size=2.5) + labs(title= paste(gene)) +  scale_fill_manual(values = colors)+ theme_classic() + theme( plot.title = element_text(hjust = 0.5, size = 11.5), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_color_manual(values= c("blue3", "goldenrod"))
#     # ggplot(gene_counts, aes(x = genotype, fill = treatment, y = counts)) +
#     #   geom_boxplot(position = "dodge") +
#     #   facet_grid(.~cell) +
#     #   scale_fill_manual(values = colors)+
#     #   theme_classic() 
#   })
# }
# shinyApp(ui, server)

### de bugging
load("temp.rds")
p = ggplot(gene_counts, aes(x= names, y= counts, fill= names)) + 
  geom_boxplot() + 
  geom_point(aes(color= cell, shape = cell),size=2.5) + labs(title= paste(gene)) +  
  # scale_fill_manual(values = colors)+ 
  theme_classic() + theme( plot.title = element_text(hjust = 0.5, size = 11.5), axis.title.x=element_blank(), axis.title.y=element_blank()) + scale_color_manual(values= c("blue3", "goldenrod"))
p
head(gene_counts)
# separate
gene_counts$genotype = sapply(strsplit(as.character(gene_counts$names), "_"), function(x)x[1])
gene_counts$treatment = sapply(strsplit(as.character(gene_counts$names), "_"), function(x)x[2])
p + facet_grid(cell~.)

table(gene_counts$names)

# This one looks better, but technically only the stroma is KO, so it doesn't make sense to separate genotype alone. 
ggplot(gene_counts, aes(x = genotype, fill = treatment, y = counts)) +
  geom_boxplot(position = "dodge") +
  facet_grid(.~cell)
