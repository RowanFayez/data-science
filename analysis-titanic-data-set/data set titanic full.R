#library for reading the csv file
installed.packages("reader")
library(reader)
data<-read.csv("C:\\Users\\Rowan\\OneDrive\\Desktop\\Titanic-Dataset.csv")
# show the structure of the data frame 
str(data)
#Display first 6 rows of the data
head(data)
#calculate the sum of the null values in columns
colSums(is.na(data))
# Use the summary() function to summarize the data
summary(data)
#eliminate or delete the duplicate values or the rows
unique(data$Pclass)
unique(data$Sex)
unique(data$Parch)
unique(data$Embarked)
# convert a vector object to a factor
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)
data$SibSp <- as.factor(data$SibSp)
data$Parch <- as.factor(data$Parch)
data$Embarked <- as.factor(data$Embarked)
data$Survived <- as.factor(data$Survived)
str(data)
#Use the summary() function to summarize the data from a Data Frame
summary(data)
#check the missing values
table(is.na(data))
table(is.na(data$Age))
table(is.na(data$Cabin))
str(data$Cabin)
unique(data$Cabin)
table(data$Sex, data$Pclass)

xtabs(formula = Fare~Pclass, data = data)

#deal with the missing values
install.packages("rpart")
library(rpart)
age.fit<- rpart(Age~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data=data[!is.na(data$Age),],method="anova")
data$Age[is.na(data$Age)] <- predict(age.fit,data[is.na(data$Age),])
table(is.na(data$Age))
table(data$Embarked)
which(data$Embarked== '')
data$Embarked[c(62,830)] = "S"
data$Embarked <- factor(data$Embarked)
unique(data$Embarked)
table(data$Embarked)
summary(data$Fare)
data$HasCabinNum <- ifelse((data$Cabin != ""), 1, 0)
summary(data$Cabin)
unique(data$cabin)
unique(data$HasCabinNum)
str(data)
str(data)

#delete columns
install.packages(dplyr)
library(dplyr)
data<- select(data,-Name)
data<- select(data,-Ticket)
data<- select(data,-Cabin)
str(data)
#calculate the probability of survival
prop.table(table(data$Survived))

#call ggplot package for visualization
install.packages("ggplot2")
library(ggplot2)
#Barplot to visualize survived vs sex
ggplot(data = data ,aes(x = Survived, fill = Sex)) +
  geom_bar() +
  geom_text(aes(label = scales::percent(..count.. / sum(..count..))), stat = 'count', position = position_stack(0.5)) +
  ggtitle("Survived vs Sex") + xlab("Survived") + ylab ("Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

#Barplot to visualize survived vs Age
#convert continues variables to categorical
data$Grouping_Age <-
  cut(data$Age, breaks <- c(seq(0, 100, by <- 18), Inf))

ggplot(data = data, aes(x = Grouping_Age, y = Survived)) +
  geom_bar(aes(fill = Survived ), stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Survived vs Age") + xlab("Age Range") + ylab ("Survived or No Survived") +
  theme_classic() + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

#Boxplot to visualize survived vs Age
ggplot(na.omit(data), aes(Survived, Age)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  theme_minimal()

#Boxplot to visualize survived vs classes
ggplot(data = data, aes(x = Pclass, y = Survived)) +
  geom_bar(aes(fill = Survived), stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  labs(title = "Ticket Class vs Survived", x = "Ticket Class", y = "Survived or No Survived") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


#graphs shows that, in both group most passengers are single.
install.packages("gridExtra")
library(gridExtra)
d1 <- ggplot(data = data, aes(Parch)) +
  geom_bar(stat = "Count") +
  theme_minimal()

d2 <- ggplot(data = data, aes(SibSp)) +
  geom_bar(stat = "Count") +
  theme_minimal()

grid.arrange(d1, d2, nrow = 1)

#piechart
par(mfrow=c(1,2))
slices<-c(129,160,573)
labels<-c("1st", "2nd", "3rd")
pie(slices, labels, main = "Pie Chart of Deathsh")

slices<-c(193,119,138)
labels<-c("1st", "2nd", "3rd")
pie(slices, labels, main="Pie Chart of Alive")

#Survival rates by age , Pclass , Sex
ggplot(data, aes(x = Age, fill = Survived)) +
  theme_bw()+
  facet_wrap(Sex ~ Pclass)+
  geom_density(alpha = 0.5)+
  labs(y = "Age",
       x = "Survived",
       title = "Titanic survival rates by age, Pclass, Sex")
#Scattered plot survival against pclass
ggplot(data, aes(x=Age, y=Fare, color=Pclass)) + geom_point() + facet_grid(Pclass~Survived)

install.packages("crayon")
library(crayon)
#gui for Visualization
install.packages("shiny")
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
install.packages("ggExtra")
library(ggExtra)
ui <- fluidPage(
  titlePanel("Titanic Dataset Visualization Dataset"),
  fluidRow(
    column(width = 4,
           
           checkboxGroupInput("pclass", "Select Pclass", choices = c("First class"=1, "Second class"=2, "Third class" = 3),selected = c(1,2,3)),
           checkboxGroupInput("sex", "Select Sex", choices = c("male","female"),selected = c("male", "female")),
           sliderInput("age", "Select Age Range", min = 0, max = 80, value = c(0, 80)),
           checkboxGroupInput("survival", "Did the passenger survive?",  choices = c("survived" = 1, "died" = 0), selected = c(1,0)),
           selectInput("xvar", "Select x-axis Variable", choices = names(data)),
           selectInput("xvar1", "X-axis Variable", choices = c("Age", "Fare")),
           selectInput("yvar", "Y-axis Variable", choices = c("Age", "Fare","Pclass")),
           selectInput("colorvar", "Color Variable", choices = c("Pclass", "Survived"))
           
    ),
    column(width = 8,
           
           tabsetPanel(type = "tab",
                       tabPanel("Data",dataTableOutput("Data")),
                       tabPanel("Plot",plotOutput("plot")),
                       tabPanel("Barplot",plotOutput("Barplot")),
                       tabPanel("Scatterplot",plotOutput("scatterplot"))
                       
           ),
           
           
           
    )
  )
)



server <- function(input, output) {
  output$Data<-renderDataTable({
    data
    
    
    
  },options=list())
  
  
  output$Barplot<-renderPlot({
    ggplot(data,aes_string(x=input$xvar))+
      theme_bw()+
      geom_bar()+
      labs(y = "passenger count", 
           title = "Titanic Survival Rates")
    
  })
  output$scatterplot <- renderPlot({
    ggplot(data, aes_string(x = input$xvar1, y = input$yvar,color = input$colorvar)) +
      geom_point() +
      facet_grid(~ Survived)
  })
  
  all_combinations <- expand.grid(Pclass = c("1", "2", "3"), Sex = c("female", "male"))
  titanic_complete <- merge(all_combinations, data, by = c("Pclass", "Sex"), all.x = TRUE)
  titanic_complete$Pclass <- as.factor(titanic_complete$Pclass)
  titanic_complete$Survived <- as.factor(titanic_complete$Survived)
  
  titanic_filtered <- reactive({
    subset(titanic_complete, (Sex == input$sex[1] | Sex == input$sex[2])& 
             (Survived == input$survival[1] | Survived == input$survival[2])& 
             Age >= input$age[1] & Age <= input$age[2]&
             (Pclass == input$pclass[1] | Pclass == input$pclass[2] | Pclass == input$pclass[3]))
    
  })
  
  
  output$plot <- renderPlot({
    ggplot(titanic_filtered(), aes(x = Age, fill = Survived)) +
      theme_bw() +
      facet_wrap(Sex ~ Pclass) +
      geom_density(alpha = 0.5) +
      labs(y = "Age",
           x = "Survived",
           title = "Titanic Survival Rates by Age,Pclass and Sex")
    
    
  }, res = 100)
}

shinyApp(ui = ui, server = server)
# --> kmeans clustering  analysis 

install.packages("factoextra") 
library(factoextra)
#Note: Save the datatype "Survived" from 'data'
data.labels <- data$Survived
# Check the length of data.labels and data
length(data.labels)
nrow(data)
# Table showing distribution of "Survived"
table(data.labels)

# Select numeric columns for clustering
numeric_cols <- sapply(data, is.numeric)
data_for_clustering <- data[numeric_cols]

# Scale the numeric data for clustering
scaled_data_for_clustering <- scale(data_for_clustering)

# Determine the optimal number of clusters using the elbow method
# Within Sum Squares
elbow_plot <- fviz_nbclust(scaled_data_for_clustering, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
# Note: Review the elbow plot to determine the optimal number of clusters
# The "elbow" usually indicates the best number of clusters (look for where the line flattens)
elbow_plot
# Applying K-means clustering with the identified optimal number of clusters
# Replace with the number of clusters from the elbow plot
k_means <- kmeans(scaled_data_for_clustering, centers = 5)
k_means
Km_clusters <-k_means$cluster
# Visualizing the clustering results using fviz_cluster
#creates a scatter plot (typically a PCA-based plot) that displays the clustering structure

rownames(scaled_data_for_clustering)<-paste(1:dim(data)[1], sep = "_")
fviz_cluster(list( data = scaled_data_for_clustering, cluster =Km_clusters))
# Checking the distribution of 'Survived' across clusters
table(Km_clusters, data$Survived)

# Check the data structure for further analysis
str(data)

#descision tree 
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
head(data)
class(data)
tree<-rpart(Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare, data = data, method ="class",minsplit=20)
#tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=data,minsplit=20)
tree
rpart.plot(tree)
new.fit<-prp(tree,snip=TRUE)$obj
prediction <- data.frame(Age = 22, Sex = "female", Pclass = "1", SibSp = "0", Parch = "0", Fare = "50", Embarked = "Q")
prediction$Age <- as.numeric(as.character(prediction$Age)) #needed
data$Fare <- as.factor(data$Fare)
prediction$Fare <- as.numeric(as.character(prediction$Fare)) #needed

#survive or not 0 or 1
predict(tree, newdata = prediction, type = "class")
#prob of each survive and not 
predict(tree, newdata = prediction)

prediction <- data.frame(Age = 60, Sex = "male", Pclass = "3", SibSp = "0", Parch = "0", Fare = "50", Embarked = "Q")
prediction$Fare <- as.numeric(as.character(prediction$Fare))
#survive or not 0 or 1
predict(tree, newdata = prediction, type = "class")
#prob of each survive and not 
predict(tree, newdata=prediction)

