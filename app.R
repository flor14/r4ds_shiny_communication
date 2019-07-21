#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(shinythemes)
library(ggthemes)
library(RColorBrewer)

ui <- fluidPage(

    # Application title
    navbarPage(id = "R4DS", "R4DS: Graphics for communication",
               theme = shinytheme("paper"),
               tabPanel("Labels",
    sidebarLayout(
        div(style = "font-size: 13px; padding: 14px 0px; margin:0%",
        sidebarPanel(p("library(ggplot2)", style = "padding: 0px 0px; margin-top:0em"), br(), 
                     p("ggplot(mpg, aes(displ, hwy)) + ", style = "padding: 0px 0px; margin-top:0em"), br(), 
                     p("geom_point(aes(color = class)) +", style = "padding: 0px 0px; margin-top:-3em"), br(),
                     p("geom_smooth(se = FALSE) +", style = "padding: 0px 0px; margin-top:-3em"), br(),   
                     p("labs(", style = "padding: 0px 0px; margin-top:-3em"), br(),       
            textInput("title", "title =", value = "Fuel efficiency generally decreases with engine size"),
            textInput("subtitle", "subtitle =", value = "Fuel efficiency generally decreases with engine size"),
            textInput("caption", "caption =", value = "Data from fueleconomy.gov"),
            textInput("x", "x =", value = "Engine displacement (L)"),
            textInput("y", "y =", value = "Highway fuel economy (mpg)"),
            textInput("colour", "colour =", value = "Car type"),
            p(")", style = "padding: 0px 0px; margin-top:0em")
        ) #sidebarpanel
        ), #div
        mainPanel(
            h5("Proba cambiar las etiquetas de este gráfico"),
            p("Para usar símbolos se agrega la función 'quote()'. Ver ?plotmath."),
           plotOutput("gra28_lab"))#mainpanel
        ) #sidebarlayout
        ), #tabpanel
    tabPanel("Annotations", 
             sidebarLayout(
                 div(style = "font-size: 13px; padding: 0px 0px; margin:0%",
              sidebarPanel(
                  p("library(dplyr)", style = "padding: 0px 0px; margin-top:0em; color:green"),br(), 
                  p("# Selecciona el auto más eficiente de su clase", style = "padding: 0px 0px; margin-top:0em"),br(), 
                  p("best_in_class <- mpg %>%", style = "padding: 0px 0px; margin-top:-3em"), br(),
                  p("group_by(class) %>%", style = "padding: 0px 0px; margin-top:-3em"), br(), 
                  p("filter(row_number(desc(hwy)) == 1)", style = "padding: 0px 0px; margin-top:-3em"), br(), 
                  p("ggplot(mpg, aes(displ, hwy)) +", style = "padding: 0px 0px; margin-top:-1em"), br(), 
                  p("geom_point(aes(colour = class)) +", style = "padding: 0px 0px; margin-top:-3em"), br(), 
                  selectInput("graph", choices = c("geom_text", "geom_label", "ggrepel::geom_label_repel"), selected = "geom_text", label = NULL),
                  p("(aes(label = model), data = best_in_class, ", style = "padding: 0px 0px; margin-top:-0.5em"),
                  numericInput("nudge_x", "nudge_x =", value = 0, width = 100),
                  numericInput("nudge_y", "nudge_y =", value = 0, width = 100),
                  sliderInput("alpha", "alpha =", min = 0, max = 1, value = 1),
                  numericInput("size", "size =", value = 6, width = 100),
                  numericInput("label_size", "label.size =", value = 0, width = 100),
                  #checkboxInput("seg_col", "segment.color =", value = "NA"),
                  selectInput("hjust", "hjust = ", choices = c("top", "center", "bottom"), selected = "center", width = 100),
                  selectInput("vjust", "vjust = ", choices = c("left", "center", "right"), selected = "center", width = 100),
                  p(")", style = "padding: 0px 0px; margin-top:0em")
                 ) #sidebarpanel
                 ), #div
              mainPanel( 
                p("Añadiendo etiquetas a mis puntos."), br(),
                p("Otros: geom_hline(), geom_vline(), geom_rect(), geom_segment()"),
                plotOutput("selected_graph")
                )# mainpanel
             ) #sidebarlayout
       ),#tabpanel
    tabPanel("Scales", #https://stackoverflow.com/questions/44159168/how-to-style-an-single-individual-selectinput-menu-in-r-shiny
             sidebarLayout(
               div(style = "font-size: 13px; padding: 14px 0px; margin:0%",
                   sidebarPanel(
                     p("ggplot(mpg, aes(displ, hwy)) +", style = "padding: 0px 0px; margin-top:-1em"), br(), 
                     p("geom_point(aes(colour = class)) +", style = "padding: 0px 0px; margin-top:-3em"), br(), 
                     p("scale_y_continous(breaks = seq(15, 40, by = 5),", style = "padding: 0px 0px; margin-top:-3em"),
                     selectInput("labels", "labels =", choices = c("seq(15, 40, by = 5)","NULL"), selected = "seq(15, 40, by = 5)"),
                     p(") +", style = "padding: 0px 0px; margin-top:0em"),
                     p("theme(", style = "padding: 0px 0px; margin-top:-1em"),
                     selectInput("legend_position", "legend.position =", choices = c("top","bottom", "right", "left", "none"), selected = "right", width = 100),
                     p(") +", style = "padding: 0px 0px; margin-top:0em"),
                     p("scale_color_brewer(", style = "padding: 0px 0px; margin-top:-1em"),
                     selectInput("palette", "palette =", choices = c("YlGn","YlGnBu",
                                                                     "GnBu", "BuGn",
                                                                     "PuBuGn", "PuBu",
                                                                     "BuPu", "RdPu", 
                                                                     "PuRd", "OrRd",
                                                                     "YlOrRd", "YlOrBr",
                                                                     "Purples", "Blues",
                                                                     "Greens", "Oranges",
                                                                     "Reds", "Greys",
                                                                     "PuOr", "BrBG",
                                                                     "PRGn", "PiYG",
                                                                     "RdBu", "RdGy",
                                                                     "RdYlBu", "Spectral",
                                                                     "RdYlGn", "Accent",
                                                                     "Dark2", "Paired",
                                                                     "Pastel1", "Pastel2",
                                                                     "Set1", "Set2", "Set3"), 
                                 selected = "Set1", width = 100),
                     p(") +", style = "padding: 0px 0px; margin-top:0em"),
                     p("guides(color = guide_legend(", style = "padding: 0px 0px; margin-top:-1em"),
                     numericInput("nrow", "nrow =", value = 7, width = 100),
                     numericInput("size_guide", "size =", value = 1, width = 100),
                     p(")", style = "padding: 0px 0px; margin-top:0em")
                     ) #sidebarpanel
               ), #div
               mainPanel( 
                 p("- 'guides' se le llama a los ejes más las leyendas", style = "padding: 0px 0px; margin-top:0em"), 
                 p("- '`breaks` and `labels` must have the same length' = deben ser un número similar!" , style = "padding: 0px 0px; margin-top:-1em"), 
                 p("- `scale_x_date()` sirve para eje x con fechas. Argumentos `date_labels` y `date_breaks`.", style = "padding: 0px 0px; margin-top:-1em"), 
                 p("- POSICION: cambios de escala directo en los ejes, ejemplo: `scale_y_log10()`", style = "padding: 0px 0px; margin-top:-1em"),
                 p("- COLOR: se puede usar la paleta viridis, ejemplo `scale_fill_viridis()`", style = "padding: 0px 0px; margin-top:-1em"),
                 p("- COLOR: la diferencia entre fill y color es color de relleno o borde", style = "padding: 0px 0px; margin-top:-1em"),
                 p("- COLOR: La terminación 'manual' para relacionarlo con valores, ejemplo: scale_color_manual()", style = "padding: 0px 0px; margin-top:-1em"),
                 plotOutput("ticks_legends")
               )# mainpanel
             ) #sidebarlayout
    ),#tabpanel
    tabPanel("Zooming",
             sidebarLayout(
               div(style = "font-size: 13px; padding: 14px 0px; margin:0%",
                   sidebarPanel(p("ggplot(mpg, aes(displ, hwy)) +", style = "padding: 0px 0px; margin-top:0em"),  
                                p("geom_point(aes(color = class)) +", style = "padding: 0px 0px; margin-top:-1em"),
                                p("geom_smooth(se = FALSE) +", style = "padding: 0px 0px; margin-top:-1em"),
                                p("coord_cartesian(", style = "padding: 0px 0px; margin-top:-1em"),
                                numericInput("xmin", "xmin =", value = 5),
                                numericInput("xmax", "xmax =", value = 7),
                                numericInput("ymin", "ymin =", value = 10),
                                numericInput("ymax", "ymax =", value = 30),
                                p(")", style = "padding: 0px 0px; margin-top:-1em")
                   ) #sidebarpanel
               ), #div
               mainPanel(
                 p("There are three ways to control the plot limits:", style = "padding: 0px 0px; margin-top:0em"),
                 p("1. Adjusting what data are plotted", style = "padding: 0px 0px; margin-top:-1em"),
                 p("2. Setting the limits in each scale" , style = "padding: 0px 0px; margin-top:-1em"),
                 p("3. Setting xlim and ylim in coord_cartesian()", style = "padding: 0px 0px; margin-top:-1em"),
                 plotOutput("zoom"))#mainpanel
             ) #sidebarlayout
    ), #tabpanel
    tabPanel("Theme",
             sidebarLayout(
                 div(style = "font-size: 13px; padding: 14px 0px; margin:0%",
                     sidebarPanel(p("ggplot(mpg, aes(displ, hwy)) +", style = "padding: 0px 0px; margin-top:0em"),  
                                  p("geom_point(aes(color = class)) +", style = "padding: 0px 0px; margin-top:-1em"), 
                                  p("geom_smooth(se = FALSE) +", style = "padding: 0px 0px; margin-top:-1em"), 
                                  selectInput("theme", "", choices = c("theme_classic()",
                                                                       "theme_light()",
                                                                       "theme_bw()",
                                                                       "theme_gray()", 
                                                                       "theme_void()",
                                                                       "theme_minimal()",
                                                                       "theme_linedraw()",
                                                                       "theme_dark()",
                                                                       "ggthemes::theme_map()",
                                                                       "ggthemes::theme_tufte()",
                                                                       "ggthemes::theme_stata()",
                                                                       "ggthemes::theme_excel_new()"),
                                                                        selected = "theme_gray()"),
                                  p(")", style = "padding: 0px 0px; margin-top:0em")
                     ) #sidebarpanel
                 ), #div
                 mainPanel(
                     p("el paquete ggthemes te ofrece más posibilidades de temas"),
                     plotOutput("plottheme"))#mainpanel
             ) #sidebarlayout
    ), #tabpanel
    tabPanel("Resize image",
             sidebarLayout(
               div(style = "font-size: 13px; padding: 14px 0px; margin:0%",
                   sidebarPanel(p("ggsave(filename = plot.png,", style = "padding: 0px 0px; margin-top:0em"),  
                                numericInput("height", "height =", value = 10),
                                numericInput("width", "width =", value = 15),
                                numericInput("dpi", "dpi =", value = 600),
                                numericInput("scale", "scale =", value = 1),
                                selectInput("units", "units =", choices = c("in", "cm", "mm"), selected = "cm"),
                                p(")", style = "padding: 0px 0px; margin-top:0em")
                ) #sidebarpanel
               ), #div
               mainPanel(
                 p("Nota como cambian el tamaño de los puntos y las letras al cambiar el tamaño de salida de la imagen", style = "padding: 0px 0px; margin-top:-1em"),
                 imageOutput("myImage"))#mainpanel
             ) #sidebarlayout
    ),#tabpanel
    tabPanel("Links",
             list(ui = fluidPage(
               uiOutput("tab"), br(), uiOutput("tab2"), br(), uiOutput("tab3") )
               )#list
    ) #tabpanel
    ) #navbarpage
)#fluidpage


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
     # Labels
    output$gra28_lab <- renderPlot({
            ggplot(mpg, aes(displ, hwy)) +
            geom_point(aes(color = class)) +
            geom_smooth(se = FALSE) +
            labs(title = input$title,
                 subtitle = input$subtitle,
                 caption = input$caption, 
                 x = input$x, 
                 y = input$y,
                 colour = input$colour)
    }) 
    
    # Labels
    
      plot1 <- reactive({
            best_in_class <- mpg %>%
                group_by(class) %>%
                filter(row_number(desc(hwy)) == 1) 
            
            ggplot(mpg, aes(displ, hwy)) +
                geom_point(aes(colour = class)) +
                geom_text(aes(label = model), data = best_in_class,
                          nudge_x = input$nudge_x,
                          nudge_y = input$nudge_y,
                          alpha = input$alpha,
                          size = input$size,
                          label.size = input$label_size,
                          #    segment.color = input$seg_col,
                          hjust = input$hjust,
                          vjust = input$vjust)
        })
        
        plot2 <- reactive({
            
            best_in_class <-    mpg %>%
                group_by(class) %>%
                filter(row_number(desc(hwy)) == 1) 
            
            ggplot(mpg, aes(displ, hwy)) +
                geom_point(aes(colour = class)) +
                geom_label(aes(label = model), data = best_in_class,
                           nudge_x = input$nudge_x,
                           nudge_y = input$nudge_y,
                           alpha = input$alpha,
                           size = input$size,
                           label.size = input$label_size,
                       #    segment.color = input$seg_col,
                           hjust = input$hjust,
                           vjust = input$vjust)
        })
        
        plot3 <- reactive({
            
            best_in_class <-    mpg %>%
                group_by(class) %>%
                filter(row_number(desc(hwy)) == 1) 
            
            ggplot(mpg, aes(displ, hwy)) +
                geom_point(aes(colour = class)) +
                geom_label_repel(aes(label = model), data = best_in_class,
                                 nudge_x = input$nudge_x,
                                 nudge_y = input$nudge_y,
                                 alpha = input$alpha,
                                 size = input$size,
                                 label.size = input$label_size,
                                 #segment.color = input$seg_col,
                                 hjust = input$hjust,
                                 vjust = input$vjust)
       
        })
        
        # Return the requested graph
        graphInput <- reactive({
            switch(input$graph,
                   "geom_text" = plot1(),
                   "geom_label" = plot2(),
                   "ggrepel::geom_label_repel" = plot3()
            )
        })
        
        output$selected_graph <- renderPlot({ 
            graphInput()
                
        })
        
        #theme
        output$plottheme <- renderPlot({ 
             
                p <- ggplot(mpg, aes(displ, hwy)) +
                    geom_point(aes(color = class)) +
                    geom_smooth(se = FALSE)
                
                if (input$theme == "theme_bw()"){
                    p <- p + theme_bw()
                }
                                    
                if (input$theme == "theme_minimal()"){
                    p <- p + theme_minimal()
                }
                
                if (input$theme == "theme_classic()"){
                    p <- p + theme_classic()
                }
                
                if (input$theme == "theme_linedraw()"){
                    p <- p + theme_linedraw()
                }
                
                if (input$theme == "theme_void()"){
                    p <- p + theme_void()
                    p
                }
                
                if (input$theme == "theme_dark()"){
                    p <- p + theme_dark()
                }
                
                if (input$theme == "theme_gray()"){
                    p <- p + theme_gray()
                }
                
                if (input$theme == "ggthemes::theme_map()"){
                    p <- p + ggthemes::theme_map()
                }
                
                if (input$theme == "ggthemes::theme_tufte()"){
                    p <- p + ggthemes::theme_tufte()       
                }
                
                if (input$theme == "ggthemes::theme_stata()"){
                    p <- p + ggthemes::theme_stata()       
                }
                
                if (input$theme == "ggthemes:theme_excel_new()"){
                    p <- p + ggthemes::theme_excel_new()       
                }
                
            p
                })
        
        # annotations
        output$ticks_legends <- renderPlot({
            best_in_class <- mpg %>%
            group_by(class) %>%
            filter(row_number(desc(hwy)) == 1)
          
         g <-   ggplot(mpg, aes(displ, hwy)) +
            geom_point(aes(colour = class)) +
           scale_colour_brewer(palette = input$palette)
            
          
          if(input$labels == "seq(15, 40, by = 5)"){
           g <- g + scale_y_continuous(breaks = seq(15, 40, by = 5), labels = seq(15, 40, by = 5))
          }
          
          if(input$labels == "NULL"){
            g <- g + scale_y_continuous(breaks = seq(15, 40, by = 5), labels = NULL)
          }
          
          if(input$legend_position == "top"){
            g <- g + theme(legend.position = "top")
          }
          
          if(input$legend_position == "bottom"){
            g <- g + theme(legend.position = "bottom")
          }
          
          if(input$legend_position == "left"){
            g <- g + theme(legend.position = "left")
          }
          
          if(input$legend_position == "right"){
            g <- g + theme(legend.position = "right")
          }
         
         g + guides(color = guide_legend(
           nrow = input$nrow,
           override.aes = list(size = input$size_guide))
         )
         
         
         
        })
     
        
        # Zooming
        output$zoom <- renderPlot({
          
          ggplot(mpg, mapping = aes(displ, hwy)) +
            geom_point(aes(color = class)) +
            geom_smooth() +
            coord_cartesian(xlim = c(input$xmin, input$xmax), 
                            ylim = c(input$ymin, input$ymax))})
      
        # Render image
          output$myImage <- renderImage({
            # Read myImage's width and height. These are reactive values, so this
            # expression will re-run whenever they change.
            width  <- session$clientData$output_myImage_width
            height <- session$clientData$output_myImage_height
            
            # A temp file to save the output.
            # This file will be removed later by renderImage
            outfile <- tempfile(fileext = '.png')
            
            # Generate the PNG
            
            ggplot(mpg, aes(displ, hwy)) + geom_point()
          ggsave(filename = outfile, 
                 scale = input$scale,
                 plot = last_plot(),
                 width = input$width,
                 height = input$height, 
                 units = input$units,
                 dpi = input$dpi 
                 )
            
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 width = width,
                 height = height,
                 alt = "This is alternate text")
          }, deleteFile = TRUE)
          
          url <- a("Capítulo comunicación de gráficos", href="https://jrnold.github.io/r4ds-exercise-solutions/graphics-for-communication.html")
          output$tab <- renderUI({
            tagList("R4DS resuelto:", url)})
          
          url2 <- a("cheatsheet", href="https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf")
          output$tab2 <- renderUI({
            tagList("ggplot2:", url2)})
          
          url3 <- a("colorbrewer", href="http://colorbrewer2.org/")
          output$tab3 <- renderUI({
            tagList("scales:", url3)})
        
}

# Run the application 
shinyApp(ui = ui, server = server)
