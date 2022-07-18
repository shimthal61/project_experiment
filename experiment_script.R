library(tidyverse)
library(ggh4x)

# create a 'graphs' folder
dir.create("graphs")

# create custom ggplot theme
my_theme <- function() {
  theme_minimal() + # use minimal theme as a starting point
    theme(aspect.ratio = 5 / 6, # use a 5/6 aspect ratio
          panel.grid.minor = element_blank(), # remove all minor gridlines
          panel.grid.major.x = element_blank(), # remove remaining vertical gridlines
          panel.grid.major.y = element_line(colour = "lightgrey"), # use light grey colour for remaining horizontal gridlines
          axis.ticks.y = element_blank(), # remove tick marks on y axis
          axis.text.y = element_text(size = 10), # set text size for text on y axis
          axis.text.x = element_text(size = 10), # set text size for text on x axis
          plot.background = element_rect(fill = "white", colour = "white"), # colour = plot border colour
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) # margins around the plotting area
}

# generate dataframe for data used in graphs
item_no <- 1:40 # numerical identifiers for each item
upper_lim <- rep_len(seq(from = 500, to = 950, by = 50), length.out = 40) # true upper limit for the data
prop <- rep_len(c(.1, .15, .2), length.out = 40) # the proportion of the upper limit that should be used as the mean when generating data
graph_data <- tibble(item_no, upper_lim, prop) # combine the above vectors into a dataframe

# read in scenarios.csv file
scenarios <- read_csv("new_scenarios_1.csv")

# join graph_data with scenarios dataframe, by item number
graph_data <- graph_data %>%
  inner_join(scenarios, by = "item_no")

# vector for x-axis labels
xlabs <- 1:5

# creating the make_plots function
# this takes a single-row dataframe (a slice() from the graph_data dataframe created above)
make_plots <- function(this_row) {
  
  # using pull() to extract single values from the dataframe
  item_no <- this_row %>% pull(item_no)
  upper_lim <- this_row %>% pull(upper_lim)
  prop <- this_row %>% pull(prop)
  seed_no <- this_row %>% pull(seed_no)
  variable <- this_row  %>% pull(variable)
  
  # generate a value for each x-axis category
  set.seed(seed_no) # use the seed number for this scenario
  mydata <- rnorm(n = length(xlabs), # number of x-axis categories
                  mean = upper_lim*prop, # sampling mean: proportion of the upper limit
                  sd = upper_lim/100) # sampling standard deviation: 
  
  # Find max value for each graph
  max_value <- max(mydata)

  # create df - a tibble with one column for x-axis values and another for y-axis values
  df <- tibble(xlabs, mydata, variable) %>%
    mutate(across(xlabs, as.character)) # treat the x-axis values as characters
  
  # create full (non-truncated) graph
  full_graph <- df %>%
    ggplot(aes(x = xlabs, # x-axis variable
               y = mydata)) + # y-axis variable
    geom_col() + # for a bar chart
    labs(x = variable, # x-axis label
         y = "Number") + # y-axis label
    scale_y_continuous(limits = c(0, upper_lim), # y axis range: between 0 and upper_lim
                       expand = expansion(mult = c(0, 0))) + # use the exact limits - don't extend limits with expansion factor
    my_theme() + # add custom theme created earlier
    geom_hline(yintercept = 0) + # add a horizontal line at 0 on the y-axis
    force_panelsizes(rows = unit(3, "cm"), cols = unit(3.5, "cm")) # function from ggh4x, to set the aspect ratio of the plotting panel
  
  # save the full graph
  full_graph %>%
    ggsave(filename = paste0("graphs/E", item_no, "full.png"), # save inside the 'graphs' folder, a
           width = 6, # width value
           height = 5, # height value
           units = "cm", # units for width and height
           dpi = 600) # dots per inch

  # create the truncated graph
  trunc_graph <- df %>%
    ggplot(aes(x = xlabs, # x-axis variable
               y = mydata)) + # y-axis variable
    geom_col() + # for a bar chart
    labs(x = variable, # x-axis label
         y = "Number") + # y-axis label
    scale_y_continuous(expand = expansion(mult = c(0, .05))) + # don't extend lower limit with expansion factor, but use default expansion factor for the upper limit
    # see default expansion value here: ggplot2:::default_expansion
    my_theme() + # add custom theme created earlier
    geom_hline(yintercept = 0) + # add a horizontal line at 0 on the y-axis
    force_panelsizes(rows = unit(3, "cm"), cols = unit(3.5, "cm")) # function from ggh4x, to set the aspect ratio of the plotting panel


  # Check to see if highest labelled gridline is higher than max value:

  # max_value <- max(mydata) # Find max value for each graph
  # trunc_max_break <- max(ggplot_build(trunc_graph)$layout$panel_params[[1]]$y$breaks,na.rm=TRUE)
  
  #if (max_value <= trunc_max_break) {
  #  cat("Problem with", item_no, "   ")
  #} else {
  #    cat("All good with", item_no, "   ")
  #}

  # Check to see if number of breaks differs between full and truncated
  trunc_breaks <- ggplot_build(trunc_graph)$layout$panel_params[[1]]$y$breaks # Creates a vector with all the truncated breaks
  full_breaks <- ggplot_build(full_graph)$layout$panel_params[[1]]$y$breaks # Creates a vector with all the full breaks
  trunc_num_breaks <- length(trunc_breaks[!is.na(trunc_breaks)]) # Counts the number of truncated breaks, ommiting any NA results
  full_num_breaks <- length(full_breaks[!is.na(full_breaks)]) # Counts the number of full breaks, ommiting any NA results

  if (trunc_num_breaks != full_num_breaks) {
      cat("Graph", item_no, "- Full Graph Breaks =", full_num_breaks, ", Trunc Graph Breaks =", trunc_num_breaks, "   ")
    } 

  if (trunc_num_breaks != full_num_breaks) {
     full_graph <- df %>%
      ggplot(aes(x = xlabs, # x-axis variable
                y = mydata)) + # y-axis variable
      geom_col() + # for a bar chart
      labs(x = variable, # x-axis label
          y = "Number") + # y-axis label
      scale_y_continuous(limits = c(0, upper_lim), # y axis range: between 0 and upper_lim
                        expand = expansion(mult = c(0, 0))) + # use the exact limits - don't extend limits with expansion factor
      my_theme() + # add custom theme created earlier
      geom_hline(yintercept = 0) + # add a horizontal line at 0 on the y-axis
      force_panelsizes(rows = unit(3, "cm"), cols = unit(3.5, "cm")) # function from ggh4x, to set the aspect ratio of the plotting panel 
  }

  # save the truncated graph
  trunc_graph %>%
    ggsave(filename = paste0("graphs/E", item_no, "trunc.png"), # save inside the 'graphs' folder, a
           width = 6, # width value
           height = 5, # height value
           units = "cm", # units for width and height
           dpi = 600) # dots per inch
  }


# apply the make_plots function to each row of graph_data
lapply(1:nrow(graph_data), function(i) make_plots(slice(graph_data, i)))

# create a tibble with the specific conditions for each item number
tibble(item_no, # use the list of item numbers created previously
       cond = rep(c("full", "trunc"), times = 20)) %>% # alternate between full and truncated version
  mutate(img = paste0("graphs/E", item_no, cond, ".png")) %>% # create 'img' column: filename for graph, based on item number and condition
  inner_join(graph_data, by = "item_no") %>% # join with materials dataframe, which contains sampling mean, upper limit, etc.
  mutate(sentence1 = str_replace_all(sentence1, " x ", " five ")) %>% # replace 'x' placeholder with actual value
  mutate(sentence1 = str_replace_all(sentence1, " y ", paste0(" ", upper_lim, " "))) %>% # replace 'y' placeholder with actual value
  write_csv("list1.csv") # write this dataframe to a .csv file