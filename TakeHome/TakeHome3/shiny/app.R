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
                selectInput(
                    inputId = "currency",
                    label = "Currency",
                    choices = list(
                        "MYR" = "Price_MYR",
                        "SGD" = "Price_SGD",
                        "USD" = "Price_USD"
                    ),
                    selected = "Price_USD"
                    # ),
                    # selectInput(
                    #     inputId = "colour",
                    #     label = "Colour scheme:",
                    #     choices = list(
                    #         "purples" = "Purples",
                    #         "blues" = "Blues",
                    #         "reds" = "Reds",
                    #         "greens" = "Greens",
                    #         "Yellow-Orange-Red" = "YlOrRd",
                    #         "Yellow-Orange-Brown" = "YlOrBr",
                    #         "Yellow-Green" = "YlGn",
                    #         "Orange-Red" = "OrRd"
                    #     ),
                    #     selected = "Purples"
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
                    "Map of all property transactions in JB and Kulai between 2023 and 2024.\nHover over a point to reveal its details."
                ),
                tmapOutput("base_map_plot",
                    width = "100%",
                    height = 580
                )
            )
        )
    ),
    tabPanel(
        "Hexagon Grid",
        sidebarLayout(
            sidebarPanel(
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
                )
            ),
            mainPanel(
                p("Map of the hexagonal grid plotted over JB and Kulai. You may select a price variable to plot or customise mapping parameters on the left panel."),
                tmapOutput("hex_grid",
                    width = "100%",
                    height = 560
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
                radioButtons(
                    inputId = "Contiguity1",
                    label = "Contiguity Method",
                    choices = c(
                        "Queen" = TRUE,
                        "Rook" = FALSE
                    ),
                    selected = "TRUE",
                    inline = TRUE
                ),
                selectInput("MoranWeights", "Spatial Weights Style",
                    choices = c(
                        "W: Row standardised" = "W",
                        "B: Binary" = "B",
                        "C: Globally standardised" = "C",
                        "U: C / no of neighbours" = "U",
                        "minmax" = "minmax",
                        "S: Variance" = "S"
                    ),
                    selected = "W"
                ),
                sliderInput(
                    inputId = "MoranSims",
                    label = "Number of Simulations:",
                    min = 99, max = 499,
                    value = 99, step = 100
                ),
                actionButton("MoranUpdate", "Update Plot"),
                hr(),
                radioButtons(
                    inputId = "MoranConf",
                    label = "Select Confidence level",
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
                        "median" = "median",
                        "pysal" = "pysal"
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
    output$base_map_plot <- renderTmap({
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
            tm_shape(property) +
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

        # Computing Contiguity Spatial Weights
        wm_q <- jb_kulai_grid %>%
            mutate(
                nb = st_contiguity(geometry,
                    queen = !!input$Contiguity1
                ), # !!: bang-bang: unquote variables inside evaluation expressions - tidyverse only
                wt = st_weights(nb,
                    style = input$MoranWeights
                )
            )

        # Computing Local Moran's I

        input_variable <- input$variable
        measured_variable <- jb_kulai_grid$median_price
        if (input_variable == "avg_price") {
            measured_variable <- jb_kulai_grid$avg_price
        } else if (input_variable == "max_price") {
            measured_variable <- jb_kulai_grid$max_price
        }

        lisa <- wm_q %>%
            mutate(
                local_moran = local_moran(
                    measured_variable, nb, wt,
                    nsim = as.numeric(input$MoranSims)
                ),
                .before = 5
            ) %>%
            unnest(local_moran)

        lisa <- lisa %>%
            rename(
                "local moran(ii)" = "ii", "expectation(eii)" = "eii",
                "variance(var_ii)" = "var_ii", "std deviation(z_ii)" = "z_ii",
                "p_value" = "p_ii"
            )



        return(lisa)
    })

    gi_statistics_results <- eventReactive(input$GiUpdate, {
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

        # }˜
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


        lisa_sig <- df %>%
            filter(p_value < as.numeric(input$MoranConf))

        lisamap <- tm_shape(df) +
            tm_polygons() +
            tm_borders() +

            tm_shape(lisa_sig) +
            tm_fill(
                col = input$LisaClass,
                palette = "-RdBu",
                title = (paste("Significance:", input$LisaClass)),
                alpha = input$opacity
            ) +
            tm_borders(alpha = 0.4) +
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
