#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(corrplot)
library(reshape2)
library(GGally)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(plotly)  

# Load the dataset
data <- read_excel("Dry_Bean_Dataset.xlsx")  

# Define the color palette
elegant_colors <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1")


sampled_data <- reactive({
  data %>%
    mutate(AspectRatio = MajorAxisLength / MinorAxisLength) %>%
    sample_n(size = 1000)
})

# Create the Shiny UI
ui <- dashboardPage(
  skin = "blue",  
  dashboardHeader(title = "Dry Bean Dataset Dashboard", titleWidth = 250),
  
  # Sidebar with navigation options
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Visualizations", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Conclusion", tabName = "conclusion", icon = icon("clipboard")),
      menuItem("Download Data", tabName = "download", icon = icon("download"))
    ),
    tags$style(HTML("
      /* Professional and attractive color scheme for the sidebar */
      .skin-blue .main-sidebar {
        background-color: #2B3E50 !important;  /* Dark slate blue */
      }
      .skin-blue .main-sidebar .sidebar a {
        color: #D3D3D3 !important;  /* Light gray for text */
      }
      .skin-blue .main-sidebar .sidebar .active a {
        background-color: #3498DB !important;  /* Light blue for active tab */
        color: #FFFFFF !important;  /* White text for the active tab */
      }
      .skin-blue .main-header .navbar {
        background-color: #1F2D3D !important;  /* Darker blue-gray for the header */
      }
    "))
  ),
  
  # Main content with three tabs
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Introduction", status = "primary", solidHeader = TRUE, width = 12,
                    p("This dashboard provides a comprehensive exploratory analysis of the Dry Bean Dataset, which is sourced from the UCI Machine Learning Repository. The dataset consists of detailed measurements of physical characteristics for seven different types of beans: BARBUNYA, BOMBAY, CALI, DERMASON, HOROZ, SEKER, and SIRA. Each of these bean types has unique properties, making the dataset valuable for studying various shape and size-related features that differentiate these bean varieties."),
                    p("The dataset includes 16 features such as Area, Perimeter, Major Axis Length, Minor Axis Length, Compactness, Shape Factors (ShapeFactor1, ShapeFactor2, ShapeFactor3, ShapeFactor4), Eccentricity, Aspect Ratio, Extent, Solidity, and EquivDiameter, among others. These features provide a rich set of attributes to explore the geometric and morphological properties of each bean type. Understanding these attributes is crucial for tasks such as classification, clustering, and pattern recognition in agricultural data science."),
                    p("The goal of this dashboard is to visualize and explore the underlying structure of the dataset by generating a series of 27 visualizations that highlight different aspects of the bean dataset. These visualizations will help uncover:"),
                    tags$li("Distribution patterns of the different features across the bean classes."),
                    tags$li("Correlations between features to identify relationships such as whether larger beans have longer perimeters or if more compact beans are generally rounder."),
                    tags$li("Class separability, which focuses on how distinct each class is in terms of their physical attributes, aiding in potential classification or prediction tasks."),
                    p("Through this interactive dashboard, users can explore the shape characteristics of the beans, observe how features vary across different classes, and analyze the relationships between key features such as size, shape, and roundness. The task of generating these visualizations is not only to display distributions and relationships but also to provide insights that could be useful in building machine learning models or performing more advanced analyses, such as feature selection or model validation.."),
                    p("Additionally, this dashboard emphasizes interactive exploration of the data, enabling users to hover over plots, zoom in on specific areas, and focus on patterns that may not be immediately obvious in static plots. Whether the goal is to study feature variability, correlations, or the overall structure of the dataset, this dashboard serves as a powerful tool for understanding the intricacies of bean classification based on their physical attributes."),
                    p("By the end of this analysis, conclusions will be drawn based on the visualizations, focusing on which features are the most distinctive between classes, how features are related to one another, and which attributes might play the most significant role in classification tasks.")
                )
              )
      ),
      
      # Visualization tab
      tabItem(tabName = "plots",
              fluidRow(
                column(3,
                       box(title = "Select Plot", status = "primary", solidHeader = TRUE, width = 12,
                           selectInput("plot_type", "Choose a Plot:", 
                                       choices = list(
                                         "Class Distribution" = "plot1",
                                         "Density Plot of Area" = "plot2",
                                         "Perimeter Boxplot" = "plot3",
                                         "Major vs Minor Axis Length Scatter" = "plot4",
                                         "Pairwise Plot" = "plot5",
                                         "Correlation Heatmap" = "plot6",
                                         "Roundness Bar Plot" = "plot7",
                                         "Compactness Boxplot" = "plot8",
                                         "Eccentricity Histogram" = "plot9",
                                         "Aspect Ratio Violin Plot" = "plot10",
                                         "EquivDiameter vs Solidity Scatter" = "plot11",
                                         "ShapeFactor1 Boxplot" = "plot12",
                                         "ShapeFactor2 Histogram" = "plot13",
                                         "ShapeFactor3 Density Plot" = "plot14",
                                         "ShapeFactor3 vs ShapeFactor4 Scatter" = "plot15",
                                         "Extent Boxplot" = "plot16",
                                         "Area vs Perimeter Scatter" = "plot17",
                                         "ConvexArea Histogram" = "plot18",
                                         "Eccentricity Boxplot" = "plot19",
                                         "Compactness Density Plot" = "plot20",
                                         "Perimeter vs Aspect Ratio Scatter" = "plot21",
                                         "ShapeFactor4 Histogram" = "plot22",
                                         "Solidity Density Plot" = "plot23",
                                         "Roundness vs Compactness Scatter" = "plot24",
                                         "EquivDiameter Boxplot" = "plot25",
                                         "ShapeFactor1 Density Plot" = "plot26",
                                         "ConvexArea vs Area Scatter" = "plot27"),
                                       selectize = TRUE)  
                       )),
                column(9,  # Made the plot column wider to give the plot more space
                       box(title = "Plot", status = "primary", solidHeader = TRUE, width = 12,
                           plotlyOutput("plot")  # Using plotly for interactive plots
                       ),
                       box(title = "Explanation", status = "info", solidHeader = TRUE, width = 12,
                           textOutput("plot_explanation")
                       )
                )
              )
      ),
      
      # Conclusion tab
      tabItem(tabName = "conclusion",
              fluidRow(
                box(title = "Conclusion", status = "primary", solidHeader = TRUE, width = 12,
                    p("The visualizations explored in this dashboard provide valuable insights into the Dry Bean Dataset. One key observation is the relationship between the features, 
                      such as the correlation between Major Axis Length and Perimeter. Larger beans tend to have longer perimeters, and this correlation is strong across all bean classes. 
                      Additionally, features such as Area and Solidity show high variability across classes, indicating that these features can be useful for distinguishing between different 
                      types of beans."),
                    p("Compactness and Roundness also show significant trends, with more compact beans being generally rounder. This relationship is more pronounced in certain classes of beans, 
                      suggesting that specific classes have more distinct physical characteristics. Overall, the visualizations highlight the potential of these features for bean classification 
                      and provide a foundation for further analysis or machine learning tasks."),
                    p("The visualizations presented in this dashboard offer deep insights into the characteristics and relationships between various features in the Dry Bean Dataset. A significant observation from the analysis is the strong correlation between the Major Axis Length and Perimeter of beans. This positive correlation indicates that beans with longer major axes tend to have larger perimeters, a pattern consistent across all bean classes. Such a correlation is particularly useful for understanding the size-related features of beans, as it suggests that perimeter, a measure of the bean's outer boundary, scales with its primary length. Furthermore, the variability observed in features such as Area and Solidity further highlights the potential for classification, as beans of different classes exhibit distinct distributions for these attributes."),
                    p("In addition to size-related features, shape-related characteristics like Compactness and Roundness also play a crucial role in distinguishing between bean classes. The relationship between these two features is particularly notable: beans that are more compact tend to be rounder, a trend that is especially visible in certain bean classes. For example, certain classes may show more pronounced roundness as their compactness increases, while others may exhibit less variation in shape. These shape-related features are critical in understanding the morphology of the beans and could serve as valuable predictors for classification purposes. For instance, distinguishing between a compact and round bean versus a more elongated bean becomes easier when examining these shape attributes."),
                    p("Overall, the visualizations in this dashboard highlight the importance of both size- and shape-related features in bean classification. The correlations, variabilities, and distinct trends observed across the dataset provide a solid foundation for further analysis, such as feature selection for machine learning models. Features like Major Axis Length, Area, Solidity, Compactness, and Roundness have shown their potential to differentiate between classes, making them strong candidates for predictive modeling. As a next step, these visual insights could be used to inform the development of classification algorithms, where features are selected and tested for their effectiveness in distinguishing between bean types. The insights drawn here lay the groundwork for more advanced tasks such as model validation, clustering, or even real-time classification of beans based on their physical attributes.")
                )
              ),
              fluidRow(
                box(title = "Non-trivial Questions and Answers", status = "primary", solidHeader = TRUE, width = 12,
                    p("Q1: What is the most significant correlation observed in the dataset?"),
                    p("Ans: From the correlation heatmap, it is evident that 'Major Axis Length' and 'Perimeter' are highly correlated, indicating that larger beans tend to have longer perimeters."),
                    p("Q2: Which feature exhibits the greatest variance across bean classes?"),
                    p("Ans: The 'Area' feature shows the greatest variance across bean classes, as shown in the density plot. This suggests that 'Area' could be a distinguishing feature for classification."),
                    p("Q3: What relationship between compactness and roundness is observed?"),
                    p("Ans: There is a noticeable trend that as the 'Compactness' of a bean increases, its 'Roundness' also increases. This relationship is more pronounced in certain bean classes, as seen in the scatter plot.")
                )
              )
      ),
      
      # Download tab
      tabItem(tabName = "download",
              fluidRow(
                box(title = "Download Dataset", status = "primary", solidHeader = TRUE, width = 12,
                    p("You can download the original dataset used in this analysis. The file is available in CSV format."),
                    downloadButton("downloadData", "Download Dry Bean Dataset")
                )
              )
      )
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output) {
  
  # Reactive expression to generate the plots
  output$plot <- renderPlotly({
    req(input$plot_type)  # Ensure a plot is selected before rendering
    
    # Render the plot based on selected type
    if (input$plot_type == "plot1") {
      p <- ggplot(sampled_data(), aes(x = Class)) + 
        geom_bar(aes(fill = Class)) + 
        labs(title = "Distribution of Bean Classes", x = "Bean Class", y = "Count") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot2") {
      p <- ggplot(sampled_data(), aes(x = Area, fill = Class)) + 
        geom_density(alpha = 0.7) + 
        labs(title = "Density Plot of Area by Bean Class", x = "Area", y = "Density") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot3") {
      p <- ggplot(sampled_data(), aes(x = Class, y = Perimeter, fill = Class)) + 
        geom_boxplot() +
        labs(title = "Boxplot of Perimeter by Bean Class", x = "Bean Class", y = "Perimeter") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot4") {
      p <- ggplot(sampled_data(), aes(x = MajorAxisLength, y = MinorAxisLength, color = Class)) +
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of Major vs Minor Axis Length", x = "Major Axis Length", y = "Minor Axis Length") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot5") {
      p <- ggpairs(sampled_data()[,c("Area", "Perimeter", "MajorAxisLength", "MinorAxisLength", "Class")], 
                   aes(color = Class, alpha = 0.6))
      ggplotly(p)
    }  else if (input$plot_type == "plot6") {
      # Interactive correlation heatmap
      corr_matrix <- cor(sampled_data() %>% select_if(is.numeric), use = "complete.obs")
      melted_corr <- reshape2::melt(corr_matrix)
      
      p <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "#00AFBB", mid = "white", high = "#FC4E07", midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation") +
        theme_minimal() +
        labs(title = "Correlation Heatmap", x = "Variables", y = "Variables") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
      
      ggplotly(p)
    }
    else if (input$plot_type == "plot7") {
      p <- ggplot(sampled_data(), aes(x = Class, y = roundness, fill = Class)) +
        geom_bar(stat = "summary", fun = "mean", position = "dodge") +
        labs(title = "Average Roundness by Bean Class", x = "Bean Class", y = "Average Roundness") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot8") {
      p <- ggplot(sampled_data(), aes(x = Class, y = Compactness, fill = Class)) + 
        geom_boxplot() + 
        labs(title = "Boxplot of Compactness by Bean Class", x = "Bean Class", y = "Compactness") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot9") {
      p <- ggplot(sampled_data(), aes(x = Eccentricity)) +
        geom_histogram(fill = "#4E79A7", bins = 30) + 
        labs(title = "Distribution of Eccentricity", x = "Eccentricity", y = "Count")
      ggplotly(p)
    } else if (input$plot_type == "plot10") {  # Aspect Ratio Violin Plot
      p <- ggplot(sampled_data(), aes(x = Class, y = AspectRatio, fill = Class)) + 
        geom_violin(trim = FALSE) +
        labs(title = "Violin Plot of Aspect Ratio by Class", x = "Bean Class", y = "Aspect Ratio") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    }else if (input$plot_type == "plot11") {
      p <- ggplot(sampled_data(), aes(x = EquivDiameter, y = Solidity, color = Class)) + 
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of EquivDiameter vs Solidity", x = "EquivDiameter", y = "Solidity") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot12") {
      p <- ggplot(sampled_data(), aes(x = Class, y = ShapeFactor1, fill = Class)) + 
        geom_boxplot() + 
        labs(title = "Boxplot of ShapeFactor1 by Class", x = "Bean Class", y = "ShapeFactor1") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot13") {
      p <- ggplot(sampled_data(), aes(x = ShapeFactor2)) +
        geom_histogram(fill = "#4E79A7", bins = 30) + 
        labs(title = "Distribution of ShapeFactor2", x = "ShapeFactor2", y = "Count")
      ggplotly(p)
    } else if (input$plot_type == "plot14") {
      p <- ggplot(sampled_data(), aes(x = ShapeFactor3, fill = Class)) +
        geom_density(alpha = 0.7) + 
        labs(title = "Density Plot of ShapeFactor3 by Class", x = "ShapeFactor3", y = "Density") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot15") {
      p <- ggplot(sampled_data(), aes(x = ShapeFactor3, y = ShapeFactor4, color = Class)) + 
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of ShapeFactor3 vs ShapeFactor4", x = "ShapeFactor3", y = "ShapeFactor4") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot16") {
      p <- ggplot(sampled_data(), aes(x = Class, y = Extent, fill = Class)) + 
        geom_boxplot() + 
        labs(title = "Boxplot of Extent by Class", x = "Bean Class", y = "Extent") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot17") {
      p <- ggplot(sampled_data(), aes(x = Area, y = Perimeter, color = Class)) + 
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of Area vs Perimeter", x = "Area", y = "Perimeter") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot18") {
      p <- ggplot(sampled_data(), aes(x = ConvexArea)) +
        geom_histogram(fill = "#4E79A7", bins = 30) + 
        labs(title = "Distribution of ConvexArea", x = "ConvexArea", y = "Count")
      ggplotly(p)
    } else if (input$plot_type == "plot19") {
      p <- ggplot(sampled_data(), aes(x = Class, y = Eccentricity, fill = Class)) + 
        geom_boxplot() + 
        labs(title = "Boxplot of Eccentricity by Class", x = "Bean Class", y = "Eccentricity") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot20") {
      p <- ggplot(sampled_data(), aes(x = Compactness, fill = Class)) +
        geom_density(alpha = 0.7) + 
        labs(title = "Density Plot of Compactness by Class", x = "Compactness", y = "Density") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot21") {
      p <- ggplot(sampled_data(), aes(x = Perimeter, y = AspectRatio, color = Class)) + 
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of Perimeter vs Aspect Ratio", x = "Perimeter", y = "Aspect Ratio") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot22") {
      p <- ggplot(sampled_data(), aes(x = ShapeFactor4)) +
        geom_histogram(fill = "#4E79A7", bins = 30) + 
        labs(title = "Distribution of ShapeFactor4", x = "ShapeFactor4", y = "Count")
      ggplotly(p)
    } else if (input$plot_type == "plot23") {
      p <- ggplot(sampled_data(), aes(x = Solidity, fill = Class)) + 
        geom_density(alpha = 0.7) + 
        labs(title = "Density Plot of Solidity by Class", x = "Solidity", y = "Density") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot24") {
      p <- ggplot(sampled_data(), aes(x = roundness, y = Compactness, color = Class)) + 
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of Roundness vs Compactness", x = "Roundness", y = "Compactness") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot25") {
      p <- ggplot(sampled_data(), aes(x = Class, y = EquivDiameter, fill = Class)) + 
        geom_boxplot() +
        labs(title = "Boxplot of EquivDiameter by Class", x = "Bean Class", y = "EquivDiameter") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot26") {
      p <- ggplot(sampled_data(), aes(x = ShapeFactor1, fill = Class)) + 
        geom_density(alpha = 0.7) + 
        labs(title = "Density Plot of ShapeFactor1 by Class", x = "ShapeFactor1", y = "Density") +
        scale_fill_manual(values = elegant_colors)
      ggplotly(p)
    } else if (input$plot_type == "plot27") {
      p <- ggplot(sampled_data(), aes(x = ConvexArea, y = Area, color = Class)) + 
        geom_point(alpha = 0.6) +
        labs(title = "Scatter Plot of ConvexArea vs Area", x = "Convex Area", y = "Area") +
        scale_color_manual(values = elegant_colors)
      ggplotly(p)
    }
  })
  
  # Provide download functionality for the dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      "Dry_Bean_Dataset.csv"
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Function to display explanations for the selected plot
  output$plot_explanation <- renderText({
    if (input$plot_type == "plot1") {
      return("This bar plot shows the distribution of different bean classes in the dataset. The x-axis represents seven distinct bean classes, while the y-axis shows the count of instances for each class. The DERMASON class has the highest count, with more than 200 instances, while BOMBAY has the lowest count, with fewer than 50. Other classes, such as BARBUNYA CALI and SIRA have moderate counts ranging from around 100 to 150. The color-coded legend helps distinguish each class visually. ")
    } else if (input$plot_type == "plot2") {
      return("This density plot shows the distribution of the Area feature for different bean classes. The x-axis represents the area values, while the y-axis indicates the density of those values for each class. Most bean classes, such as BARBUNYA, SIRA, and SEKER, have their area values concentrated around 50,000. In contrast, BOMBAY exhibits a broader range of area values, with a peak around 150,000. The overlapping density curves highlight the differences and similarities in area distribution between the bean classes. This plot is useful for understanding how the area varies among the different types of beans.")
    } else if (input$plot_type == "plot3") {
      return("This boxplot illustrates the distribution of the Perimeter feature for different bean classes. Each bean class is represented along the x-axis, while the perimeter values are shown on the y-axis. The boxes represent the interquartile range (IQR), with the middle line indicating the median. BOMBAY has the highest perimeter values, with a larger spread compared to other classes. DERMASON, HOROZ, and SIRA have smaller ranges, while BARBUNYA and SEKER have lower perimeter values overall. Outliers, shown as dots, indicate beans whose perimeter values differ significantly from the rest of the class.")
    } else if (input$plot_type == "plot4") {
      return("This scatter plot visualizes the relationship between the Major Axis Length and Minor Axis Length for different bean classes. Each point represents an instance, with the color indicating its respective bean class. The x-axis shows the major axis length, while the y-axis represents the minor axis length. BOMBAY beans (orange) have noticeably larger major and minor axis lengths compared to the other classes, clustering at the higher end of both axes. Other classes, like BARBUNYA, DERMASON, and SIRA, are more concentrated at the lower ranges of both axis lengths")
    } else if (input$plot_type == "plot5") {
      return("The pairwise plot displays the relationships between multiple variables for each class, helping to visualize potential correlations and interactions between the features. It's particularly useful to identify patterns across several variables at once.")
    } else if (input$plot_type == "plot6") {
      return("This correlation heatmap shows the relationships between different variables in the dataset, with color indicating the strength and direction of correlations. Red represents strong positive correlations, such as between Area, Perimeter, MajorAxisLength, and MinorAxisLength, while blue indicates negative correlations, like those between ShapeFactor1 and variables such as Area and Perimeter. The intensity of the color reflects the strength of the correlation, with deeper colors indicating stronger relationships.")
    } else if (input$plot_type == "plot7") {
      return("This bar plot shows the average Roundness values for each bean class. The x-axis lists the different bean classes, while the y-axis represents their average roundness, ranging from 0 to 1. All classes have roundness values close to 0.75, with minor differences among them. DERMASON and SEKER have slightly higher average roundness, while HOROZ has the lowest.")
    } else if (input$plot_type == "plot8") {
      return("This boxplot displays the distribution of Compactness for different bean classes. The x-axis represents the bean classes, while the y-axis shows the compactness values. Each box shows the interquartile range (IQR), with the line inside indicating the median. DERMASON and SEKER have the highest compactness medians, while HOROZ and CALI show lower compactness values. Outliers, represented by dots, are present in classes like SEKER and HOROZ, indicating beans with unusually high or low compactness values compared to others. ")
    } else if (input$plot_type == "plot9") {
      return("This histogram displays the distribution of Eccentricity values in the dataset. The x-axis represents eccentricity values, while the y-axis shows the count of instances for each value range. Most of the data points are concentrated between 0.6 and 0.8, with the highest count occurring around 0.7. There are fewer instances at lower eccentricity values (below 0.5) and higher values (above 0.9). This plot highlights that most bean instances have moderately high eccentricity, indicating their deviation from being perfectly circular.")
    } else if (input$plot_type == "plot10") {
      return("This violin plot shows the distribution of 'Aspect Ratio' across different bean classes. The aspect ratio measures the relationship between the bean's major and minor axis lengths. This plot helps visualize the shape characteristics of beans across classes.")
    } else if (input$plot_type == "plot11") {
      return("This scatter plot shows the relationship between EquivDiameter and Solidity for different bean classes. The x-axis represents the equivalent diameter (EquivDiameter), while the y-axis shows the solidity values. Each point represents a bean instance, with colors differentiating the bean classes. Most points cluster in the range of EquivDiameter between 200 and 300, and Solidity values are generally high, ranging between 0.97 and 0.99. BOMBAY beans (orange) are spread farther along the EquivDiameter axis, having higher values compared to other classes. This plot highlights how solidity changes relative to the equivalent diameter across different bean types.")
    } else if (input$plot_type == "plot12") {
      return("This boxplot visualizes the distribution of ShapeFactor1 across different bean classes. The x-axis represents the bean classes, and the y-axis shows the values for ShapeFactor1. Each box represents the interquartile range (IQR) with the median marked inside, while the whiskers extend to show the range of the data. DERMASON and HOROZ have higher median values for ShapeFactor1, while BOMBAY has the lowest median. Outliers, shown as dots, are observed in classes like SIRA and DERMASON, indicating beans with unusual ShapeFactor1 values.")
    } else if (input$plot_type == "plot13") {
      return("This histogram illustrates the distribution of ShapeFactor2 in the dataset. The x-axis represents the values of ShapeFactor2, while the y-axis shows the count of instances for each value range. The distribution is slightly skewed to the right, with the highest count occurring around 0.001. The frequency decreases as ShapeFactor2 increases beyond this point, with fewer instances having values above 0.002. ")
    } else if (input$plot_type == "plot14") {
      return("This density plot shows the distribution of ShapeFactor3 for each bean class. The x-axis represents the values of ShapeFactor3, and the y-axis indicates the density of the data. HOROZ has the lowest ShapeFactor3 values, peaking around 0.5, while SEKER has the highest, peaking near 0.8. Classes like CALI and SIRA have overlapping distributions between 0.6 and 0.7. This plot helps compare how ShapeFactor3 values are distributed across the different bean classes, highlighting both similarities and differences.")
    } else if (input$plot_type == "plot15") {
      return("This scatter plot shows the relationship between ShapeFactor3 and ShapeFactor4 for different bean classes. The x-axis represents ShapeFactor3, while the y-axis represents ShapeFactor4, with points color-coded by bean class. Most data points cluster around higher ShapeFactor4 values (close to 1.0), while ShapeFactor3 varies from 0.5 to 0.9. SEKER beans (yellow) tend to have the highest ShapeFactor3 values, while HOROZ (green) has a broader spread across lower values. ")
    } else if (input$plot_type == "plot16") {
      return("This boxplot compares the distribution of Extent across different bean classes. The x-axis represents the bean classes, and the y-axis shows the extent values. The boxes indicate the interquartile range (IQR), with the middle line representing the median. HOROZ has the widest range of extent values, while other classes like BOMBAY and CALI have more compact distributions around higher values. SEKER has one outlier with a lower extent value.")
    } else if (input$plot_type == "plot17") {
      return("This scatter plot visualizes the relationship between Area and Perimeter across different bean classes. The x-axis represents the Area and the y-axis represents the Perimeter. Each point corresponds to a bean instance, with different colors representing various bean classes. The plot shows a clear positive correlation between area and perimeter, meaning as the area of the bean increases, so does its perimeter. BOMBAY beans (orange) have larger areas and perimeters compared to other classes. ")
    } else if (input$plot_type == "plot18") {
      return("This histogram shows the distribution of ConvexArea values across the dataset. The x-axis represents the convex area values, while the y-axis shows the count of instances for each value range. The distribution is right-skewed, with most instances clustered between 50,000 and 100,000 convex area. The highest count is around 75,000, while there are fewer instances with larger convex areas, extending beyond 200,000.")
    } else if (input$plot_type == "plot19") {
      return("This boxplot shows the distribution of Eccentricity for different bean classes. The x-axis represents the bean classes, and the y-axis shows the eccentricity values. Each box represents the interquartile range (IQR), with the line inside indicating the median. HOROZ and SIRA have higher median eccentricity values compared to other classes, while SEKER shows the lowest median with a wider range and several outliers. DERMASON also has many outliers on the lower end.")
    } else if (input$plot_type == "plot20") {
      return("This density plot shows the distribution of Compactness for each bean class. The x-axis represents the compactness values, while the y-axis shows the density of data points. Each bean class is represented by a different color. HOROZ has the lowest compactness values, peaking around 0.7, while SEKER has the highest compactness values, peaking near 0.9. CALI and SIRA overlap around the middle compactness values (close to 0.8), indicating some similarity in their distributions.")
    } else if (input$plot_type == "plot21") {
      return("This scatter plot shows the relationship between 'Perimeter' and 'Aspect Ratio' across different bean classes. It helps us see if there's a connection between the perimeter and the overall shape of the beans for each class.")
    } else if (input$plot_type == "plot22") {
      return("This histogram illustrates the distribution of ShapeFactor4 values across the dataset. The x-axis represents the ShapeFactor4 values, while the y-axis shows the count of instances for each value range. The distribution is heavily right-skewed, with most data points concentrated near 1.0. The highest count occurs around values close to 1.0, while very few instances have ShapeFactor4 values below 0.97.")
    }  else if (input$plot_type == "plot23") {
      return("This density plot visualizes the distribution of Solidity across different bean classes. The x-axis represents solidity values, while the y-axis shows the density of data points for each class. Most bean classes, such as BARBUNYA and SEKER, have high solidity values clustering around 0.99, indicating strong solidity for most instances. Classes like BOMBAY have lower solidity values with a broader spread. The plot highlights that most classes are tightly clustered with high solidity, while a few, such as BOMBAY, have slightly lower values.")
    } else if (input$plot_type == "plot24") {
      return("This scatter plot shows the relationship between Roundness and Compactness for different bean classes. The x-axis represents Roundness, while the y-axis shows Compactness. Each point represents a bean instance, with colors indicating the bean class. There is a clear positive correlation between roundness and compactness: as roundness increases, compactness also tends to increase. SEKER beans (yellow) generally have the highest roundness and compactness, while HOROZ beans (green) have lower values for both.")
    } else if (input$plot_type == "plot25") {
      return("This boxplot illustrates the distribution of EquivDiameter across different bean classes. The x-axis represents the bean classes, and the y-axis shows the equivalent diameter values. BOMBAY beans have the largest median EquivDiameter with a wider range, while SEKER and SIRA beans have smaller median values and tighter distributions. BARBUNYA has a few outliers on the lower end, and CALI shows more variability compared to other classes.")
    } else if (input$plot_type == "plot26") {
      return("This density plot shows the distribution of ShapeFactor1 for different bean classes. The x-axis represents the ShapeFactor1 values, and the y-axis indicates the density of instances for each class. BOMBAY beans (orange) have a distinct peak around 0.004, indicating most of their ShapeFactor1 values are clustered in this range. Other classes, such as CALI and SIRA, overlap around 0.005, while DERMASON and HOROZ have higher ShapeFactor1 values, peaking closer to 0.008. ")
    } else if (input$plot_type == "plot27") {
      return("This scatter plot shows the relationship between ConvexArea and Area for different bean classes. The x-axis represents ConvexArea, while the y-axis shows Area. Each point represents a bean instance, with colors indicating the bean class. There is a strong positive linear correlation between the two variables, meaning as the convex area increases, the regular area also increases proportionally. BOMBAY beans (orange) have higher values for both ConvexArea and Area compared to other classes, while the rest of the classes cluster together in the lower range. ")
    }
  })
  # Function to answer non-trivial questions
  output$question_answer <- renderText({
    if (input$question == "q1") {
      return("From the correlation heatmap, it is evident that 'Major Axis Length' and 'Perimeter' are highly correlated, indicating that larger beans tend to have longer perimeters.")
    } else if (input$question == "q2") {
      return("The 'Area' feature shows the greatest variance across bean classes, as shown in the density plot. This suggests that 'Area' could be a distinguishing feature for classification.")
    } else if (input$question == "q3") {
      return("There is a noticeable trend that as the 'Compactness' of a bean increases, its 'Roundness' also increases. This relationship is more pronounced in certain bean classes, as seen in the scatter plot.")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



