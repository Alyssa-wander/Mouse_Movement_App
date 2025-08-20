library(sp)
library(imputeTS)
library(ggplot2)
library(ggmap)
library(data.table)
library(cowplot)
library(corrplot)
library(dplyr)
library(MASS)
library(purrr)
library(RColorBrewer)
library(tibble)
library(tidyr)
library(ggpubr)
library(egg)
library(grid)
library(kableExtra)
library(magrittr)
library(gt)
library(webshot2)
library(xtable)
library(knitr)
library(tinytex)
library(stringr)
library(ggpmisc)
library(ggtext)
library(fs)
library(stringi)
library(jsonlite)
library(shinyjs)
library(tools)
library(assertthat)
library(plotrix)
library(rlang)
library(shinydashboard)
library(shinythemes)
library(broom)
library(reactable)
library(Hmisc)
library(stats)
library(gtable)
library(shinydashboardPlus)
library(shinyWidgets)
library(rhandsontable)
library(rio)
library(janitor)
library(flextable)
library(lubridate)
library(shiny)
library(plotly)
library(gridExtra)
library(shinybusy)
library(DT)
library(future)
library(promises)
library(reticulate)


# Increase upload size limit:
options(shiny.maxRequestSize = 1024 * 1024^2)  # 100 MB
rainbow_colors <- c(
  "#F09EA7", "#F6CA94", "#FAFABE",
  "#C1EDC0", "#C7CAFF", "#CDABEB", "#F6C2F3"
)

# required python packages
required_packages <- c(
  "opencv-python-headless", # for cv2
  "numpy",
  "pandas",
  "matplotlib",
  "scipy",
  "fpdf"
)

# setup miniconda + install any missing packages
try({
  miniconda_python_path <- reticulate::miniconda_path()
  
  if (!dir.exists(miniconda_python_path)) {
    reticulate::install_miniconda()
  }
  
  # Force R to use Miniconda's environment
  use_condaenv("r-reticulate", conda = file.path(miniconda_path(), "bin", "conda"), required = TRUE)
  
  missing <- required_packages[!sapply(required_packages, py_module_available)]
  if (length(missing) > 0) {
    py_install(missing, envname = "r-reticulate", pip = TRUE)
  }
  
  message("Miniconda Python environment is ready.")
}, silent = FALSE)

# Load external functions:
source('DLCAnalyzer_Functions_final.R')

## Define UI for the Shiny app:
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"),
               HTML(".shiny-notification {
                                              position:fixed;
                                              top: calc(10%);
                                              left: calc(33%);
                                              font-size: 150%;
                                              border: black 1px solid;
                                              background-color: #f7f2d9;
                                              width: 28%;}"
               ))
  ),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        fluidRow(
          column(1,
                 img(src="Abimael_3000.jpeg",
                     width = 120,
                     height = 150,
                     class = "pull-right"
                 )),
          column(10,
                 h1("The Abimael Laboratory of Neurometabolism"),
                 h4(tags$i("We want to beat disability")))# This text is italicized by using tags$i
        )
      )
  ),
  
  fluidRow(
    column(12,
           hr(),
           h2("Mouse behavior analysis", style = "color:blue")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width= 2,
      
      h4(span(icon("microscope"), "Bottom Video:")),
      numericInput("num_groups", "Number of Groups:", 2, min = 1),
      uiOutput("groupNamesInputs"),
      uiOutput("fileInputs"),
      # Center the button in the sidebar
      div(style = "text-align: center;",
          actionButton("runBottomAnalysis", label = HTML(paste(icon("database"), "<br>Body/Movement Analysis")), 
                       style = "color: white; background-color: blue; width: 100%; height: 50px;")
          ),
      
      hr(),
      
      h4(span(icon("video"), " Top Video:")),
      fileInput("top_files", "Upload Top Files", multiple = TRUE,
                accept = c(".csv", ".mp4")),
      div(style = "text-align: center;",
          actionButton("runTopVideosAnalysis", label = HTML(paste(icon("video-camera"), "<br>Top Videos Analysis")),
                       style = "color: white; background-color: blue; width: 100%; height: 50px;" )
          )
    ),
    
    mainPanel(
      fluidRow(
        column(12,
               div(id = "instruction",
                   h4("Instructions"),
                   tags$ul(
                     div(style = "margin-bottom: 10px;",
                         tags$li(tags$b("Ensure that ℹ️ DLCAnalyzer_Functions_final.R and ℹ️ dlc_sway_analysis.py are present in the working directory."))
                     ),
                     div(style = "margin-bottom: 10px;",
                       tags$li(tags$b("Bottom Video Analysis: ")),
                       tags$p("  🔬This tool analyzes mouse body features, speed, distance, and zone visits using DeepLabCut-generated .csv files."),
                       tags$p("  📊Upload grouped DLC outputs to visualize metrics, plots, and summaries for multiple cohorts.")
                     ),
                     div(style = "margin-bottom: 10px;",
                       tags$li(tags$b("Top Videos Analysis:")),
                       tags$p("  🎥️This analysis generates annotated videos, sway plots, and statistical summaries comparing control vs mutant groups."),
                       tags$p("  📁 Upload matched DeepLabCut CSV and video files."),
                       tags$p("  ✅ CSV filenames must contain 'control' or 'mutant' and a number (e.g., 'mutant 3')."),
                       tags$p("  ✅ Video files must match the same group + number as their CSV (e.g., 'mutant 3.mov').")
                     ),
                     div(style = "margin-bottom: 10px;",
                         tags$li(tags$b("Video Demo: ")),
                         tags$p(" 🎬 A quick introduction to the Mouse Behavior Analysis App."),
                         tags$video(
                           src = "ReadME_Mouse_Movement_Demo.mp4",
                           type = "video/mp4",
                           controls = "controls",
                           width = "100%"
                         )
                         )
                   ),
                   hr()
                   )
               )
        ),
      # Use tabsetPanel to create tabs
      tabsetPanel(
        id = "mainTabs",
        tabPanel("Metrics (mice area, length, etc.)",
                 # Place the elements for Metrics analysis here
                 uiOutput("bodyMetrics_plotly"),
                 div(id = "downloadPlotsDiv", downloadButton("download_bodyMetrics_plots", "Download Plots")),
                 # results table display
                 DT::dataTableOutput("bodyMetricsTable"),
                 div(id = "downloadTableDiv", downloadButton("download_bodyMetrics_Table", "Download Results Table"))
        ),
        tabPanel("Movement metrics (speed, distance, etc.)",
                 uiOutput('movement_plotly'),
                 div(id = "downloadPlotsDiv_2", downloadButton("download_movement_plots", "Download Plots")),
                 DTOutput("movement_table"),
                 div(id = "downloadTableDiv_2", downloadButton("download_movement_table", "Download Results Table"))
        ),
        # Movement metrics tab
        tabPanel("Tracking",
                 br(),
                 div(style = "margin-bottom: 10px;",
                     downloadButton("download_zone_density", "Download Zone Visits & Density Plots")),
                 br(),  # Add some space before the new download button
                 uiOutput("group_tab") # dynamic groups will appear here
                 
        ),
        tabPanel("Top-video: Body Sway",
                 tableOutput("top_file_preview"),
                 br(),
                 h4("📄 Sway summary"),
                 tableOutput("sway_table"),
                 br(),
                 h4("\U0001F3A5 Video Previews"),
                 br(),
                 uiOutput("top_download_buttons_ui"),
                 uiOutput("video_preview_ui"),
                 
                 div(
                   id = "video_spinner",
                   style = "display:none; text-align: center; padding: 20px;",
                   icon("cogs", class = "fa-spin fa-2x"),
                   tags$p(id = "progress_text", "Processing video..."),
                   tags$p(id = "time_estimate", "")
                 )
                 )
      ),
      # Adjust the width ratio to give more space to the main panel
      widths = c(3, 9)
    )
  ),
  tags$style(type = "text/css", ".tab-content {overflow-y: scroll;}")
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    body_static_boxplots = NULL,
    combined_body_metrics = NULL,
    TrackingAllbygroup = NULL,
    combined_movement = NULL,
    movement_boxplots = NULL,
    tracking_results = list(),
    tracking_combined = list(),
    tracking_combined_plot = NULL,
    zone = list(),
    density = list()
  )
  
  all_data <- list()  # To store data from all groups
  TrackingAll <- list()
  
  
  # Dynamically generate textInput for group names
  output$groupNamesInputs <- renderUI({
    lapply(1:input$num_groups, function(i) {
      textInput(paste0("group_name", i), paste0("Name for Group ", i), value = paste("Group", i))
    })
  })
  
  # Dynamically generate fileInput based on number of groups
  output$fileInputs <- renderUI({
    lapply(1:input$num_groups, function(i) {
      fileInput(paste0("file", i), paste0("Upload Files for ", input[[paste0("group_name", i)]]), multiple = TRUE)
    })
  })
  
  
  
  # Initially hide the download buttons
  shinyjs::hide("downloadtrackingPlotsDiv")
  shinyjs::hide("downloadData")
  shinyjs::hide("downloadPlotsDiv_2")
  shinyjs::hide("downloadTableDiv_2")
  shinyjs::hide("downloadPlotsDiv")
  shinyjs::hide("downloadTableDiv")
  
  
  observeEvent(input$runBottomAnalysis, {
    shinyjs::hide("instruction")
    req(input$num_groups)
    n_groups <- input$num_groups
    groupName <- sapply(seq_len(input$num_groups), function(i) input[[paste0("group_name", i)]])
    
    # rest tracking completion flag
    rv$tracking_complete <- FALSE
    
    # =============================================
    # PART 1: Immediate Execution (Body Metrics & Movement)
    # =============================================
    
    # show modal progress dialog
    show_modal_spinner(spin = "atom",
                       text = "Processing body metrics...")
    
    # part I: body metrics and movement analysis
    all_body_metrics <- list() # initiate an empty list to store all group data
    all_movement <- list()
    zone_list <- setNames(vector("list", n_groups), groupName)
    density_list <- setNames(vector("list", n_groups), groupName)
    
    for (i in 1:n_groups) {
      files <- input[[paste0("file", i)]]
      
      # update progress
      update_modal_spinner(text = paste("Processing body metrics of Group", i, " - ", nrow(files), "files..."))
      
      if (!is.null(files)) {
        
        # preserve original names
        TrackingAll <- withCallingHandlers(
          {
            RunPipeline(files$datapath, FUN = pipeline)
            
          },
          warning = function(w) {
            if (grepl("no label data present", conditionMessage(w))) {
              invokeRestart("muffleWarning") # Suppress warning "no label data present" (it's OK)
            }
          }
        )
        names(TrackingAll) <- files$name
        rv$TrackingAllbygroup[[groupName[[i]]]] <- TrackingAll
        
        
        # ============ 1st tab - body metrics
        
        body_metrics <- calculate_body_metrics(TrackingAll) # group = "Control|Mutant" extract from file name
        body_metrics$group <- groupName[[i]] # assign the group name to the data
        
        # store the results for this group with descriptive name
        all_body_metrics[[i]] <- body_metrics
        
        # ============ running 2rd tab - movement analysis
        update_modal_spinner(text = paste("Processing movements of Group", i, " - ", nrow(files), "files..."))
        movement <- calculate_movement(TrackingAll)
        movement$group <- groupName[[i]]
        all_movement[[groupName[[i]]]] <- movement
        
        # ============ 3rd tab - tracking - zone & density
        zone_list[[groupName[[i]]]] <- plot_time_in_zones(TrackingAll, groupName[[i]], colors = rainbow_colors)
        density_list[[groupName[[i]]]] <- generate_density_plots(TrackingAll, groupName[[i]])
        
        # ============ 3rd tab - tracking - consolidated
        results <- apply_all_functions(TrackingAll)
        results$df$group <- groupName[[i]]  # Assign the group name to the data
        all_data[[groupName[[i]]]] <- results$df  # Store data from each group
        
      }
    }
    
    
    consolidated_data <- do.call(rbind, all_data)
    print(consolidated_data) # Check the first few rows of the combined data frame
    
    # combine all groups into one data frame
    rv$combined_body_metrics <- do.call(rbind, all_body_metrics)
    
    # update progress (body_metrics boxplots)
    update_modal_spinner(text = "Preparing boxplots of bodyMetrics...")
    
    # update reactive list zone & density
    rv$zone <- zone_list
    rv$density <- density_list
    
    # create boxplot
    # y_label list
    rv$body_static_boxplots <- create_static_boxplot(rv$combined_body_metrics, 
                                                     y_label_map = BodyMetrics_y_label,
                                                     fill_colors = rainbow_colors)
    
    # render interactive boxplots
    render_boxplots_interactive(rv$body_static_boxplots, output_id_prefix = "body_plotly")
    
    # 2rd tab - movement
    rv$combined_movement <- do.call(rbind, all_movement)
    
    # update progress (movement boxplots)
    update_modal_spinner(text = "Preparing boxplots of movement data...")
    
    # create boxplot
    # y_label list outsource
    rv$movement_boxplots <- create_static_boxplot(rv$combined_movement,
                                                  y_label_map = movement_y_label,
                                                  fill_colors = rainbow_colors)
    
    # render interactive boxplots
    render_boxplots_interactive(rv$movement_boxplots, output_id_prefix = "movement_plotly")
    
    # Show the download buttons for bodyMetrics and movement
    shinyjs::show("downloadPlotsDiv_2")
    shinyjs::show("downloadTableDiv_2")
    shinyjs::show("downloadPlotsDiv")
    shinyjs::show("downloadTableDiv")
    
    # =============================================
    # PART 2: Async Execution (Tracking Analysis)
    # =============================================
    
    update_modal_spinner(text = "Starting Tracking analysis...")
    
    # dynamically generate subpanel on tracking panel based on number of groups
    output$group_tab <- renderUI({
      req(input$num_groups)
      
      
      # Create tabs for each group
      tabs <- lapply(1:n_groups, function(i) {
        tabPanel(
          title = paste("Group", i),
          # fluidRow inside each tab: left for zone plots, right for density
          fluidRow(
            column(
              width = 6,
              uiOutput(paste0("group", i, "_zone_plots"))
            ),
            column(
              width = 6,
              uiOutput(paste0("group", i, "_density_plots"))
            )
          )
        )
      })
      
      # add the result tab at the end
      tabs <- c(tabs, list(
        tabPanel(
          title = "Results",
          fluidRow(
            column(7, plotOutput("combined_plot"),
                   div(style = "margin-top: 20px;", downloadButton("download_combined_plots", "Download Plots"))),
            column(5, DTOutput("tracking_table"),
                   div(style = "margin-top: 20px;", downloadButton("download_tracking_table", "Download Data")))
          ),
          fluidRow(
            column(12, 
                   h4("Coombined Tracking Summary"),
                   DTOutput("tracking_combined"),
                   div(style = "margin-top: 20px;", downloadButton("download_tracking_combined", "Download Data")))
          )
        )
      ))
      do.call(tabsetPanel, tabs)
    }) # end of tabs
    
    
    # third tab (must wrapped in the observeEvent as the reactive value)
    
    lapply(1:n_groups, function(i) {
      # zone plots
      output[[paste0("group", i, "_zone_plots")]] <- renderUI({
        plot_output_list <- lapply(1:length(zone_list[[i]]), function(j) {
          plotname <- paste0("group", i, "_zone", j)
          plotOutput(plotname, height = "500px")
        })
        do.call(tagList, plot_output_list)
      })
      # render individual plots
      lapply(1:length(zone_list[[i]]), function(j) {
        output[[paste0("group", i, "_zone", j)]] <- renderPlot({
          zone_list[[i]][[j]]
        })
      })
      
      
      # density plots
      output[[paste0("group", i, "_density_plots")]] <- renderUI({
        group_plots <- density_list[[groupName[[i]]]]
        
        plot_output_list <- lapply(seq_along(group_plots), function(j) {
          plotname <- paste0("group", i, "_density", j)
          plotOutput(plotname, height = "500px")
        })
        do.call(tagList, plot_output_list)
      })
      
      lapply(seq_along(density_list[[groupName[[i]]]]), function(j) {
        output[[paste0("group", i, "_density", j)]] <- renderPlot({
          print(density_list[[groupName[[i]]]][[j]])
        })
      })
    })
    
    
    # =============================================
    # start async processing
    
    future({
      # loading required package
      # you can consider future is a new R session
      library(lubridate)
      library(plotly)
      library(promises)
      
      # isolate reactive dependencies
      num_groups <- isolate(input$num_groups)
      groupName <- sapply(1:n_groups, function(i) isolate(input[[paste0("group_name", i)]]))
      tracking_data <- isolate(rv$TrackingAllbygroup)
      
      selected_points <- c("Head")
      selected_zones <- c("center", "periphery")
      
      analysis_results <- list()
      
      for (i in 1:num_groups) {
        # run analysis and plot generation on the group's data
        group_analysis_result <- analyze_tracking(
          tracking_data[[groupName[[i]]]], selected_points, selected_zones, groupName[[i]]
        )
        
        analysis_results[[groupName[[i]]]] <- group_analysis_result$summary
      }
      
      if (length(analysis_results) > 0) {
        combined_analysis <- data.frame()
        
        for (groupName in names(analysis_results)) {
          combined_analysis <- rbind(combined_analysis, analysis_results[[groupName]])
        }
        
        
        combined_analysis_grouped <- combined_analysis %>%
          group_by(group, zone) %>%
          summarise(mean_percentage = mean(percentage), sd_percentage = sd(percentage), .groups = "drop")
        
        combined_analysis_grouped <- combined_analysis_grouped %>%
          mutate(p_value = NA_real_, significance = NA_character_)
        
        all_posthoc_results <- data.frame()
        
        for (zone_name in unique(combined_analysis$zone)) {
          data_sub <- combined_analysis %>% filter(zone == zone_name)
          
          if (length(unique(data_sub$group)) == 2 && all(table(data_sub$group) >= 3)) {
            t_test_results <- t.test(percentage ~ group, data = data_sub)
            combined_analysis_grouped <- combined_analysis_grouped %>%
              mutate(p_value = if_else(zone == zone_name, t_test_results$p.value, p_value),
                     significance = if_else(zone == zone_name, ifelse(p_value < 0.05, "p < 0.05", "ns"), significance))
          } else if(length(unique(data_sub$group)) > 2) {
            aov_results <- aov(percentage ~ group, data = data_sub)
            if(!is.null(aov_results)) {
              posthoc_results <- TukeyHSD(aov_results, "group")
              if("group" %in% names(posthoc_results)) {
                posthoc_df <- as.data.frame(posthoc_results$group) %>%
                  tibble::rownames_to_column(var = "comparison") %>%
                  tidyr::separate(comparison, into = c("group1", "group2"), sep = "-") %>%
                  dplyr::mutate(zone = zone_name, p_adj_value = p.adjust(`p adj`, method = "bonferroni"))
                
                # Append the results of this zone to the cumulative dataframe
                all_posthoc_results <- rbind(all_posthoc_results, posthoc_df)
              }
            }
          }
        }
      }
      
      
      
      
      # return raw data (no rendering here)
      list(
        analysis_results = analysis_results,
        combined_analysis = combined_analysis,
        posthoc_results = all_posthoc_results,
        combined_bygroup = combined_analysis_grouped
      )
    }) %...>% 
      (function(res) {
        rv$tracking_results <- res$posthoc_results
        rv$tracking_combined <- res$combined_analysis
        if (nrow(res$combined_bygroup) > 0) {
          combined_plot <- ggplot(res$combined_bygroup, aes(x = zone, y = mean_percentage, fill = group)) +
            geom_errorbar(aes(ymin = mean_percentage - sd_percentage, ymax = mean_percentage + sd_percentage),
                          width = 0.2, position = position_dodge(0.9)) +
            geom_bar(stat = "identity", position = position_dodge(), colour = "gray40") +
            scale_fill_manual(values = rainbow_colors) +
            theme_bw() +
            labs(x = "Zone", y = "Mean Percentage of Frames (%)", fill = "Group") +
            geom_text(aes(label = significance), position = position_dodge(width = 0.9), vjust = -1.5, check_overlap = TRUE) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
          # Only create the table if more than two groups are compared
          if (length(unique(res$combined_bygroup$group)) > 2) {
            rv$tracking_results <- rv$tracking_results %>%
              dplyr::select(group1, group2, zone, p_adj_value)
            
            
          }
          
          rv$tracking_combined_plot <- combined_plot
        } else {
          print("The combined_analysis_grouped data frame is empty.")
        }
        
        
        
        
      }) # end of future
    
    
    remove_modal_spinner()
    
    
    
    
  }) # end of observeEvent/runBottomAnalysis

  # =============================================
  # Top Video: Body Sway
  # ============================================= 
  
  # create "www/uploads" at run time
  upload_dir <- "www/uploads"
  csv_dir <- file.path(upload_dir, "csv")
  output_dir <- "www/results"
  dir.create(upload_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # clean up entire uploads directory on app exit
  onStop(function() {
    if (dir.exists(upload_dir)) unlink(upload_dir, recursive = TRUE)
    if (dir.exists(output_dir)) unlink(output_dir, recursive = TRUE)
  })

  observeEvent(input$top_files, {
    req(input$top_files)
    
    # save all uploaded video and csv files to temp folder
    for (i in 1:nrow(input$top_files)) {
      ext <- tolower(tools::file_ext(input$top_files$name[i]))
      save_dir <- if (ext == "csv") csv_dir else upload_dir
      file.copy(input$top_files$datapath[i],
                file.path(save_dir, input$top_files$name[i]),
                overwrite = TRUE)
    }
  })
  
  # reactive value for top video
  full_sway_results <- reactiveVal(NULL)
  
  observeEvent(input$runTopVideosAnalysis, {
    shinyjs::hide("instruction")
    
    csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
    mp4_files <- list.files(upload_dir, pattern = "\\.(mov|mps|mp4)$", full.names = TRUE)
    
    # ⬆Step 1: if either type is missing
    if (length(csv_files) == 0 || length(mp4_files) == 0) {
      showModal(modalDialog(
        title = "🚫 Missing Files",
        "You must upload both CSV and video files to continue.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    # ⬆Step 2: if counts don't match, return early
    if (length(csv_files) != length(mp4_files)) {
      showModal(modalDialog(
        title = "❌ File Count Mismatch",
        paste("You uploaded", length(csv_files), "CSV files and ", length(mp4_files), "video files."),
        "Please make sure the number of files matches.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(invisible(NULL))
    }
    
    if (!reticulate::py_available(initialize = TRUE)) {
      showNotification("Python environment failed to initialize. Please restart the app.",
                       type = "error")
    }
    # source Python
    source_python("dlc_sway_analysis.py")
    
    # ⬆Step 3: Run Python matching function
    matched <- match_csv_to_videos(csv_files, mp4_files)
    
    matched_csv <- unlist(lapply(matched, `[[`, 1))
    matched_mp4 <- unlist(lapply(matched, `[[`, 2))
    
    unmatched_csv <- setdiff(csv_files, matched_csv)
    unmatched_mp4 <- setdiff(mp4_files, matched_mp4)
    
    # stop execution if unmatched files exist
    if (length(unmatched_csv) > 0 || length(unmatched_mp4) > 0) {
      unmatched_df <- data.frame(
        Type = c(rep("Unmatched CSV", length(unmatched_csv)), rep("unmatched Video", length(unmatched_mp4))),
        File = c(basename(unmatched_csv), basename(unmatched_mp4))
      )
      
      output$unmatchedTable <- renderTable(
        unmatched_df,
        striped = TRUE, bordered = TRUE, hover = TRUE
      )
      
      showModal(modalDialog(
        title = "❌ Unmatched Files",
        tagList(
          p("Please upload the matching files and try again."),
          tableOutput("unmatchedTable")
        ),
        easyClose = TRUE
      ))
      return(invisible(NULL))
    } else {
      showModal(modalDialog(
        title = "✅ All Files Matched",
        "CSV and video files successfully paired.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    
    summary_df <- data.frame()
    video_preview <- c()
    lateral_list <- list()
    movement_list <- list()
    
    # ⬆Step 4: Estimate time for processing videos
    seconds_per_byte <- 2.7e-7
    start_time <- Sys.time()
    for (i in seq_along(matched_csv)) {
      # remaining processing time based on the file size
      remaining_bytes <- sum(sapply(matched_mp4[i:length(matched_mp4)],
                                    function(p) file.info(p)$size))
      remaining_sec <- ceiling(remaining_bytes * seconds_per_byte)
      eta_text <- paste0(remaining_sec, "seconds left")
      js_text <- sprintf("
                         $('#video_spinner').show(); 
                         $('#progress_text').text('Processing video %d / %d');
                         $('#time_estimate').text('~%s');
                         ", i, length(matched_mp4), eta_text
                         )
      runjs(js_text)
      
      # ⬆Step 5: load and clean csv files
      csv_path <- matched_csv[i]
      video_path <- matched_mp4[i]
      filename <- tools::file_path_sans_ext(basename(csv_path))
      output_filename <- paste0(filename, "_overlay.mp4")
      output_path <- file.path(output_dir, output_filename)
      
      # ⬆Step 6: Python analysis pipeline
      df <- load_csv_r_style(csv_path)
      df <- clean_tracking_data(df)
      sway_result <- calculate_CoM_and_sway_for_selected_parts(df)
      CoM_x <- sway_result[[1]]
      CoM_y <- sway_result[[2]]
      sway_mag <- sway_result[[3]]
      lateral_sway <- sway_result[[4]]
      
      concatenate_video_and_CoM_sway_from_df(
        video_path = video_path,
        cleaned_data = df,
        output_video_path = output_path,
        plot_pdf_path_combined = file.path(output_dir, paste0(filename, "_plot.pdf"))
      )
      
      movements <- rep(TRUE, length(lateral_sway))
      mean_val <- calculate_sway_means(lateral_sway, movements)
      std_val <- calculate_std_dev(lateral_sway)
      
      lateral_list[[length(lateral_list) + 1]] <- lateral_sway
      movement_list[[length(movement_list) + 1]] <- movements
      
      summary_df <- rbind(summary_df, data.frame(
        File = filename,
        Mean = mean_val,
        SD = std_val,
        stringsAsFactors = FALSE
      ))
      
      video_preview <- c(video_preview, output_filename)
    
    } # end of for loop (i in seq_along(matched_csv))
    runjs("$('#video_spinner').hide();")
    
    summary_df$lateral_sway <- lateral_list
    summary_df$movements <- movement_list
    
    full_sway_results(summary_df)
    
    # ⬆Step 8: video preview
    output$video_preview_ui <- renderUI({
      tagList(lapply(video_preview, function(file) {
        tags$div(
          tags$h5(file),
          tags$video(
            src = file.path("results", file),
            type = "video/mp4",
            controls = NA,
            width = "100%"
          )
        )
      }))
    })
    
    # ⬆Step 9: set up the video and pdf download btn
    shorten <- function(x) {
      gsub("[^a-zA-Z0-9_]", "_", x)
    }
    output$top_download_buttons_ui <- renderUI({
      tagList(
        lapply(video_preview, function(vfile) {
          vname <- tools::file_path_sans_ext(vfile) #take the filename vfile and remove its file extension
          shorten_name <- shorten (vname)
          
          tagList(
            downloadButton(
              outputId = paste0("dl_video_", shorten_name),
              label = tagList(icon("file"), "Download", shorten_name, ".mp4")
            ),
            downloadButton(
              outputId = paste0("dl_pdf_", shorten_name),
              label = tagList(icon("file-pdf"), 
                              paste0("Download", sub("_overlay", "_plot", shorten_name), ".pdf")
                              )
            )
          )
        })
      )
    })
    
    # set up downloadHandler for both videos and pdf
    lapply(video_preview, function(vfile) {
      vname <- tools::file_path_sans_ext(vfile)
      shorten_name <- shorten(vname)
      
      # video download handler
      output[[paste0("dl_video_", shorten_name)]] <- downloadHandler(
        filename = function() paste0(vname, "_", Sys.Date(), ".mp4"),
        content = function(file) {
          file.copy(file.path(output_dir, vfile), file)
        },
        contentType = "video/mp4"
      )
      
      # PDF download handler
      pfile <- sub("_overlay\\.mp4$", "_plot.pdf", vfile)
      output[[paste0("dl_pdf_", shorten_name)]] <- downloadHandler(
        filename = function() paste0(vname, "_", Sys.Date(), ".pdf"),
        content = function(file) {
          file.copy(file.path(output_dir, pfile), file)
        },
        contentType = "application/pdf"
      )
    })
    
  }) # end of observeEvent runTopVideosAnalysis
  

  
  
  
  
  
  
  
  # Render the table in the main panel
  output$bodyMetricsTable <- renderDT({
    datatable(rv$combined_body_metrics,
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })
  
  output$download_bodyMetrics_Table <- downloadHandler(
    filename = function() {
      paste("bodyMetrics_results_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$combined_body_metrics)
      write.csv(rv$combined_body_metrics, file, row.names = FALSE)
    }
  )
  
  
  output$bodyMetrics_plotly <- renderUI({
    req(rv$body_static_boxplots)
    plot_output_list <- lapply(seq_along(rv$body_static_boxplots), function(i) {
      column(width = 4,
             plotlyOutput(paste0("body_plotly", i), height = "300px")
      )
    })
    # arrange in rows with 3 plots each
    rows <- split(plot_output_list, ceiling(seq_along(rv$body_static_boxplots)/3))
    rows <- lapply(rows, function(row) {
      fluidRow(do.call(tagList, row))
    })
    do.call(tagList, rows)
  })
  
  
  # PDF generation handler
  output$download_bodyMetrics_plots <- downloadHandler(
    filename = function() {
      paste0("body_metrics_boxplots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(rv$body_static_boxplots)
      save_boxplots_to_pdf(rv$body_static_boxplots, file, title = "Body Metrics Summary")
    }
  )
  
  ## second tab : movement
  # Render the table in the main panel
  output$movement_table <- renderDT({
    datatable(rv$combined_movement,
              options = list(scrollX = TRUE, pageLength = 10),
              rownames = FALSE)
  })
  
  
  output$download_movement_table <- downloadHandler(
    filename = function() {
      paste("Movement_results_table_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$combined_movement)
      write.csv(rv$combined_movement, file, row.names = FALSE)
    }
  )
  
  
  output$movement_plotly <- renderUI({
    req(rv$movement_boxplots)
    plot_output_list <- lapply(seq_along(rv$movement_boxplots), function(i) {
      column(width = 4,
             plotlyOutput(paste0("movement_plotly", i), height = "300px")
      )
    })
    # arrange in rows with 3 plots each
    rows <- split(plot_output_list, ceiling(seq_along(rv$movement_boxplots)/3))
    rows <- lapply(rows, function(row) {
      fluidRow(do.call(tagList, row))
    })
    do.call(tagList, rows)
  })
  
  
  # PDF generation handler
  output$download_movement_plots <- downloadHandler(
    filename = function() {
      paste("movement_boxplots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(rv$movement_boxplots)
      save_boxplots_to_pdf(rv$movement_boxplots, file, title = "Movement Summary")
    }
  )
  
  # 3rd tab
  output$combined_plot <- renderPlot({
    req(rv$tracking_combined_plot)
    rv$tracking_combined_plot
  })
  
  output$download_combined_plots <- downloadHandler(
    filename = function() {
      paste0("combined_tracking_results_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(rv$tracking_combined_plot, rv$tracking_results)
      
      df <- rv$tracking_results
      df$p_adj_value <- round(df$p_adj_value, 3)
      
      # convert table to grob
      table_grob <- gridExtra::tableGrob(rv$tracking_results, rows = NULL)
      
      # open PDF
      pdf(file, width = 8.5, height = 11)
      on.exit(dev.off())
      
      # arrange plot and table on same page
      gridExtra::grid.arrange(
        rv$tracking_combined_plot,
        table_grob,
        ncol = 1,
        heights = c(2/3, 1/3) # adjust split as needed
      )
    }
  )
  
  output$tracking_table <- renderDT({
    req(rv$tracking_results)
    datatable(rv$tracking_results, options = list(pageLength = 10))
  })
  
  output$download_tracking_table <- downloadHandler(
    filename = function() {
      "posthoc_results.csv"
    },
    content = function(file) {
      write.csv(rv$tracking_results, file, row.names = FALSE)
    }
  )
  
  output$tracking_combined <- renderDT({
    req(rv$tracking_combined)
    datatable(rv$tracking_combined)
  })
  
  output$download_tracking_combined <- downloadHandler(
    filename = function() {
      "tracking_analysis.csv"
    },
    content = function(file) {
      write.csv(rv$tracking_combined, file, row.names = FALSE)
    }
  )
  
  output$download_zone_density <- downloadHandler(
    filename = function() {
      paste0("zone_density_plots_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(rv$zone, rv$density)
      
      pdf(file = file, width = 11, height = 8.5) # US letter
      on.exit(dev.off())
      
      max_pairs_per_page <- 2
      
      for (group in names(rv$zone)) {
        zone_plots <- rv$zone[[group]]
        density_plots <- rv$density[[group]]
        n <- length(zone_plots)
        
        total_pages <- ceiling(n / max_pairs_per_page)
        
        for (page in seq_len(total_pages)) {
          start <- (page - 1) * max_pairs_per_page + 1
          end <- min(page * max_pairs_per_page, n)
          
          # get plots for this page
          zone_subset <- zone_plots[start:end]
          density_subset <- density_plots[start:end]
          
          # pad to ensure 2 full pairs
          n_missing <- max_pairs_per_page - length(zone_subset)
          if (n_missing > 0) {
            padding <- req(list(nullGrob()), n_missing)
            zone_subset <- c(zone_subset, padding)
            density_subset <- c(density_subset, padding)
          }
          
          # create paired layout (each row = zone + density)
          paired_row <- lapply(seq_len(max_pairs_per_page), function(i) {
            arrangeGrob(zone_subset[[i]], density_subset[[i]], ncol = 2)
          })
          
          # Title for the page
          title_grob <- textGrob(
            paste("Zone Visits & Density Plots -", group),
            gp = gpar(fontsize = 16, fontface = "bold")
          )
          full_page <- arrangeGrob(
            title_grob,
            do.call(arrangeGrob, c(paired_row, list(ncol = 1))),
            ncol = 1,
            heights = unit(c(0.7, 7.5), "in")
          )
          if (!(group == names(rv$zone)[1] && page == 1)) {
            grid.newpage()
          }
          grid.draw(full_page)
        }
        
      }
    }
  )
  # =============================================
  # Top Video: Body Sway
  # ============================================= 
  
  # show uploaded files in a table
  output$top_file_preview <- renderTable({
    req(input$top_files)
    preview <- input$top_files[, c("name", "type", "size")]
    preview$size <- round(preview$size / 1024^2, 2) # conver to MB
    names(preview)[names(preview) == "size"] <- "size_MB"
    preview
  })
  
  output$sway_table <- renderTable({
    req(full_sway_results())
    full_sway_results()[, c("File", "Mean", "SD")]
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)