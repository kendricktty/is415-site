#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(
    shiny, bslib, shinydashboard, shinythemes, rsconnect, olsrr, ggstatsplot, sf, tmap, tidyverse, gtsummary, performance, see, sfdep, spdep, tidygeocoder, RColorBrewer
)

adm3_jb_kulai <- read_rds("data/rds/adm3_jb_kulai.rds")
jb_kulai_grid <- read_rds("data/rds/jb_kulai_grid.rds")
property <- read_rds("data/rds/property_preprocessed.rds")
zoom_limits <- c(10, 25)
default_num_classes <- 6

large_map_size <- 720
med_map_size <- 640
adj_map_size <- 560

property_type_names <- c(
                        "1 - 1 1/2 Storey Semi-Detached",
                        "1 - 1 1/2 Storey Terraced",
                        "2 - 2 1/2 Storey Semi-Detached",
                        "2 - 2 1/2 Storey Terraced",
                        "Cluster House",
                        "Condominium",
                        "Detached",
                        "Townhouse"
                    )

property_types <- c(
                        "1 - 1 1/2 Storey Semi-Detached",
                        "1 - 1 1/2 Storey Terraced",
                        "2 - 2 1/2 Storey Semi-Detached",
                        "2 - 2 1/2 Storey Terraced",
                        "Cluster House",
                        "Condominium/Apartment",
                        "Detached",
                        "Town House"
                    )

# ========================#
###### Shiny UI ######
# ========================#

ui <- navbarPage(
    title = "JB: The New Frontier?",
    fluid = TRUE,
    theme = shinytheme("flatly"),
    id = "navbarID",
    tabPanel(
        "Base Map",
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput(
                    inputId = "types_to_keep",
                    label = "Include: ",
                    choiceNames = property_type_names,
                    choiceValues = property_types,
                    selected = property_types
                ),
                selectInput(
                    inputId = "currency",
                    label = "Currency",
                    choices = list(
                        "MYR" = "Price_MYR",
                        "SGD" = "Price_SGD",
                        "USD" = "Price_USD"
                    ),
                    selected = "Price_MYR"
                ),
                sliderInput(
                    inputId = "classes",
                    label = "Number of classes",
                    min = 2,
                    max = 20,
                    value = c(default_num_classes)
                ),
                selectInput(
                    inputId = "classification",
                    label = "Classification method:",
                    choices = list(
                        "sd" = "sd",
                        "equal" = "equal",
                        "pretty" = "pretty",
                        "quantile" = "quantile",
                        "kmeans" = "kmeans"
                    ),
                    selected = "kmeans"
                ),
                actionButton("BasePlotUpdate", "Update Plot"),
                hr(),
                sliderInput(
                    inputId = "opacity",
                    label = "Level of transparency",
                    min = 0,
                    max = 1,
                    value = c(0.5)
                )
            ),
            mainPanel(
                p(
                    "Map of all property transactions in JB and Kulai between 2023 and 2024. Hover over a point to reveal its details."
                ),
                tmapOutput("base_map_plot",
                    width = "100%",
                    height = (560 * 2)
                )
            )
        )
    ),
    tabPanel(
        "Hexagon Grid",
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput(
                    inputId = "types_to_keep",
                    label = "Include: ",
                    choiceNames = c(
                        "1 - 1 1/2 Storey Semi-Detached",
                        "1 - 1 1/2 Storey Terraced",
                        "2 - 2 1/2 Storey Semi-Detached",
                        "2 - 2 1/2 Storey Terraced",
                        "Cluster House",
                        "Condominium",
                        "Detached",
                        "Townhouse"
                    ),
                    choiceValues = c(
                        "1 - 1 1/2 Storey Semi-Detached",
                        "1 - 1 1/2 Storey Terraced",
                        "2 - 2 1/2 Storey Semi-Detached",
                        "2 - 2 1/2 Storey Terraced",
                        "Cluster House",
                        "Condominium/Apartment",
                        "Detached",
                        "Town House"
                    )
                ),
                selectInput(
                    inputId = "variable",
                    label = "Mapping variable",
                    choices = list(
                        "Density" = "density",
                        "Average Property Prices" = "avg_price",
                        "Median Property Prices" = "median_price",
                        "Maximum Property Prices" = "max_price"
                    ),
                    selected = "median_price"
                ),
                selectInput(
                    inputId = "classification",
                    label = "Classification method:",
                    choices = list(
                        "sd" = "sd",
                        "equal" = "equal",
                        "pretty" = "pretty",
                        "quantile" = "quantile",
                        "kmeans" = "kmeans"
                    ),
                    selected = "kmeans"
                ),
                sliderInput(
                    inputId = "classes",
                    label = "Number of classes",
                    min = 2,
                    max = 20,
                    value = c(default_num_classes)
                    # ),
                    # selectInput(
                    #     inputId = "colour",
                    #     label = "Colour scheme:",
                    #     choices = list(
                    #         "blues" = "Blues",
                    #         "reds" = "Reds",
                    #         "greens" = "Greens",
                    #         "Yellow-Orange-Red" = "YlOrRd",
                    #         "Yellow-Orange-Brown" = "YlOrBr",
                    #         "Yellow-Green" = "YlGn",
                    #         "Orange-Red" = "OrRd"
                    #     ),
                    #     selected = "YlOrRd"
                ),
                sliderInput(
                    inputId = "opacity",
                    label = "Level of transparency",
                    min = 0,
                    max = 1,
                    value = c(0.5)
                ),
                actionButton("HexUpdate", "Update Plot"),
                hr()
            ),
            mainPanel(
                p("Map of the hexagonal grid plotted over JB and Kulai. You may select a price variable to plot or customise mapping parameters on the left panel."),
                tmapOutput("hex_grid",
                    width = "100%",
                    height = large_map_size
                )
            )
        )
    ),
    navbarMenu(
        "Global Measures",
        tabPanel("Moran's I"), # not defined yet
        tabPanel("Geary's c"), # not defined yet
        tabPanel("Getis-Ord Global G") # not defined yet
    ),
    tabPanel(
        "Local Moran",
        # tabPanel("Local Moran")
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "variable",
                    label = "Subject variable",
                    choices = list(
                        "Average Property Prices" = "avg_price",
                        "Median Property Prices" = "median_price",
                        "Maximum Property Prices" = "max_price"
                    ),
                    selected = "median_price"
                ),
                actionButton("MoranUpdate", "Update Plot"),
                hr(),
                radioButtons(
                    inputId = "MoranConf",
                    label = "Confidence level",
                    choices = c(
                        "0.95" = 0.05,
                        "0.99" = 0.01
                    ),
                    selected = 0.05,
                    inline = TRUE
                ),
                selectInput("LisaClass", "Lisa Classification",
                    choices = c(
                        "mean" = "mean",
                        "median" = "median"
                    ),
                    selected = "mean"
                ),
                selectInput("localmoranstats", "Output variable",
                    choices = c(
                        "local moran(ii)" = "local moran(ii)",
                        "expectation(eii)" = "expectation(eii)",
                        "variance(var_ii)" = "variance(var_ii)",
                        "std deviation(z_ii)" = "std deviation(z_ii)",
                        "P-value" = "p_value"
                    ),
                    selected = "local moran(ii)"
                ),
                sliderInput(
                    inputId = "opacity",
                    label = "Level of transparency",
                    min = 0,
                    max = 1,
                    value = c(0.5)
                )
            ),
            mainPanel(
                p("The selected output variable map will be displayed on the left, and the LISA class map on the right. Customise the parameters in the left panel, then click 'Update Plot' to begin."),
                fluidRow(
                    column(6, tmapOutput("LocalMoranMap")),
                    column(6, tmapOutput("LISA"))
                ) # Maximum total width is 12
                # Use 6 and 6 to define equal distance
                # Can have a map with a statistical output
            )
        )
        # tabPanel("Local Gi")
    ),
    tabPanel(
        "Local Gi",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    inputId = "variable",
                    label = "Subject variable",
                    choices = list(
                        "Average Property Prices" = "avg_price",
                        "Median Property Prices" = "median_price",
                        "Maximum Property Prices" = "max_price"
                    ),
                    selected = "median_price"
                ),
                radioButtons(
                    inputId = "BandwidthType",
                    label = "Bandwidth",
                    choices = c(
                        "Fixed" = FALSE,
                        "Adaptive" = TRUE
                    ),
                    selected = "TRUE",
                    inline = TRUE
                ),
                sliderInput(
                    inputId = "k",
                    label = "k-means K value (adaptive only)",
                    min = 2, max = 15,
                    value = 2, step = 1
                ),
                selectInput("GiWeights", "Spatial Weights Style",
                    choices = c(
                        "W: Row standardised" = "W",
                        "B: Binary" = "B",
                        "C: Globally standardised" = "C",
                        "U: C / no of neighbours" = "U",
                        "minmax" = "minmax",
                        "S: Variance" = "S"
                    ),
                    selected = "B"
                ),
                actionButton("GiUpdate", "Update Plot"),
                hr(),
                sliderInput(
                    inputId = "opacity",
                    label = "Level of transparency",
                    min = 0,
                    max = 1,
                    value = c(0.5)
                )
            ),
            mainPanel(
                p("The hot/cold spot map will be displayed on the left, and the price map on the right. Customise the parameters in the left panel, then click 'Update Plot' to begin."),
                fluidRow(
                    column(6, tmapOutput("Gi")),
                    column(6, tmapOutput("hex_grid_2"))
                ) # Maximum total width is 12
                # Use 6 and 6 to define equal distance
                # Can have a map with a statistical output
            )
        )
    )
)


# ========================#
###### Shiny Server ######
# ========================#

server <- function(input, output) {
    filter_properties <- reactive({
        # Filter data based on checkboxes
        property %>% filter(`Property Type` %in% input$types_to_keep)
    })



    output$base_map_plot <- renderTmap({
        property_filtered <- filter_properties()
        if (is.null(property_filtered)) {
            tmap_options(check.and.fix =  TRUE) +
                tm_shape(adm3_jb_kulai) +
                tm_view(
                    set.zoom.limits = zoom_limits # Hardcoded; reduces resource requirements
                ) + 
                tm_basemap("OpenStreetMap")
            return()
        }
        print(paste("Creating base map plot for currency variable:", input$currency))
        currency <- input$currency
        colour <- "Purples"
        if (currency == "Price_SGD") {
            colour <- "Reds"
        } else if (currency == "Price_MYR") {
            colour <- "YlOrBr"
        }

        tmap_options(check.and.fix = TRUE) +
            tm_shape(adm3_jb_kulai) +
            tm_polygons(alpha = 0.4) +
            tm_shape(property_filtered) +
            tm_dots(
                col = input$currency,
                alpha = 0.6,
                style = input$classification,
                palette = colour,
                n = input$classes,
                popup.vars = c(
                    "Scheme Name/Area", "Road Name", "Mukim", "District",
                    "Month, Year of Transaction Date", "Land/Parcel Area", "Main Floor Area",
                    "Price_MYR", "Price_SGD", "Price_USD"
                )
            ) +
            tm_view(
                set.zoom.limits = zoom_limits # Hardcoded; reduces resource requirements
            ) + tm_basemap("OpenStreetMap")
    })



    hex_grid <- renderTmap({
        input_variable <- input$variable
        print(paste("Creating hex grid plot for mapping variable:", input_variable))
        colour <- "Greens"
        if (input_variable == "avg_price") {
            colour <- "Purples"
        } else if (input_variable == "max_price") {
            colour <- "YlOrRd"
        } else if (input_variable == "density") {
            colour <- "Greys"
        }

        tmap_options(check.and.fix = TRUE) +
            tm_shape(jb_kulai_grid) +
            tm_fill(input$variable,
                n = input$classes,
                style = input$classification,
                palette = colour,
                alpha = input$opacity
            ) +
            tm_borders(lwd = 0.1, alpha = 1) +
            tm_view(
                set.zoom.limits = zoom_limits # Hardcoded; reduces resource requirements
            ) + tm_basemap("OpenStreetMap")
    })

    output$hex_grid <- hex_grid
    output$hex_grid_2 <- hex_grid

    # ==========================================================
    # Local Measures of Spatial AutoCorrelation
    # ==========================================================

    localMIResults <- eventReactive(input$MoranUpdate, {
        if (nrow(jb_kulai_grid) == 0) {
            return(NULL)
        } # Exit if no data



        # Compute distance weights
        # Grids with null values are removed - need to account for "islands"
        jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
        longitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[1]])
        latitude <- map_dbl(jb_kulai_wgs$geometry, ~st_centroid(.x)[[2]])
        coords <- cbind(longitude, latitude)
        # Determine the cutoff distance
        k1 <- knn2nb(knearneigh(coords))
        k1dists <- unlist(nbdists(k1, coords, longlat = TRUE))
        print(summary(k1dists))

        # Compute adaptive distance weight matrix
        max_distance <- max(k1dists)
        nb <- dnearneigh(coords, 0, max_distance, longlat=TRUE)
        distances <- nbdists(nb, coords)
        rswm_q <- nb2listw(nb, glist = distances, style = "W")

        # Compute local Moran's I
        fips <- order(jb_kulai_grid$index)
        localMI <- localmoran(jb_kulai_grid$median_price, rswm_q)

        # Merge into dataset
        lisa <- cbind(jb_kulai_grid, localMI) %>%
            rename(
                "local moran(ii)" = "Ii",
                "expectation(eii)" = "E.Ii",
                "variance(var_ii)" = "Var.Ii",
                "std deviation(z_ii)" = "Z.Ii",
                "p_value" = "Pr.z....E.Ii.."
            )
        
        quadrant <- vector(mode = "numeric", length = nrow(localMI))
        lisa$lag <- lag.listw(rswm_q, jb_kulai_grid$median_price)
        DV <- lisa$lag - mean(jb_kulai_grid$median_price)
        
        print(paste("Lisa Class:", input$LisaClass))
        if (input$LisaClass == "median") {
            LM_I <- localMI[,1] - median(localMI[, 1])
        } else {
            LM_I <- localMI[,1] - mean(localMI[, 1])
        }

        signif <- input$MoranConf
        quadrant[DV <0 & LM_I>0] <- 1
        quadrant[DV >0 & LM_I<0] <- 2
        quadrant[DV <0 & LM_I<0] <- 3  
        quadrant[DV >0 & LM_I>0] <- 4
        quadrant[localMI[,5]>signif] <- 0
        lisa$quadrant_class <- quadrant
        return(lisa)
    })

    gi_statistics_results <- eventReactive(input$GiUpdate, {
        print(paste("Number of rows in JB grid", nrow(jb_kulai_grid)))
        if (nrow(jb_kulai_grid) == 0) {
            return(NULL)
        } # Exit if no data
        jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
        longitude <- map_dbl(jb_kulai_wgs$geometry, ~ st_centroid(.x)[[1]])
        latitude <- map_dbl(jb_kulai_wgs$geometry, ~ st_centroid(.x)[[2]])
        coords <- cbind(longitude, latitude)
        print("Successfully created coordinates")

        use_adaptive_bandwidth <- as.logical(input$BandwidthType)
        # if (use_adaptive_bandwidth) {

        # }
        knn <- knn2nb(knearneigh(coords, k = 8))
        knn_lw <- nb2listw(knn, style = input$GiWeights)

        input_variable <- input$variable
        measured_variable <- jb_kulai_grid$median_price
        if (input_variable == "avg_price") {
            measured_variable <- jb_kulai_grid$avg_price
        } else if (input_variable == "max_price") {
            measured_variable <- jb_kulai_grid$max_price
        }

        gi <- localG(measured_variable, knn_lw)
        return(gi)
    })

    # ==========================================================
    # Render output maps
    # ==========================================================

    # Render local Moran I statistics
    output$LocalMoranMap <- renderTmap({
        print("Creating local Moran's I map")
        df <- localMIResults()

        if (is.null(df) || nrow(df) == 0) {
            return()
        } # Exit if no data

        # Map creation using tmap
        localMI_map <- tm_shape(df) +
            tm_fill(
                col = input$localmoranstats,
                style = "pretty",
                palette = "RdBu",
                title = input$localmoranstats,
                alpha = input$opacity
            ) +
            tm_borders() +
            tm_view(set.zoom.limits = zoom_limits) + tm_basemap("OpenStreetMap")

        localMI_map
    })

    # Render LISA map
    output$LISA <- renderTmap({
        print("Creating LISA cluster map")
        df <- localMIResults()
        if (is.null(df)) {
            return()
        }
        print(names(df))
        colors <- c("#ffffff", "#2c7bb6", "#fdae61", "#abd9e9", "#d7191c")
        clusters <- c("insignificant", "low-low", "low-high", "high-low", "high-high")
        lisamap <- tm_shape(df) +
            tm_fill(
                col = "quadrant_class",
                style = "cat", 
                palette = colors[c(sort(unique(df$quadrant_class)))+1], 
                labels = clusters[c(sort(unique(df$quadrant_class)))+1],
                title = (paste("Quadrant")),
                alpha = input$opacity
            ) +
            tm_borders() +
            tm_view(set.zoom.limits = zoom_limits) + tm_basemap("OpenStreetMap")

        lisamap
    })

    # Render hot/cold spot map
    output$Gi <- renderTmap({
        print("Creating hot/cold spot map")
        gi <- gi_statistics_results()
        if (is.null(gi)) {
            return()
        }
        jb_kulai_wgs <- jb_kulai_grid %>% st_transform(4326)
        jb_kulai.gi <- cbind(jb_kulai_wgs, as.matrix(gi)) %>% rename(gstat = as.matrix.gi.)
        gi_map <- tm_shape(jb_kulai.gi) +
            tm_fill(
                col = "gstat",
                palette = "-RdBu",
                title = "Local Gi",
                breaks = seq(from = -10, to = 10, by = 2),
                alpha = input$opacity
            ) +
            tm_borders(alpha = 0.5) +
            tm_view(set.zoom.limits = zoom_limits) + tm_basemap("OpenStreetMap")
        gi_map
    })
}

shinyApp(ui = ui, server = server, options = list(port = 4000))
