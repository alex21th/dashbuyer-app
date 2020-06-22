
options(scipen = 999) # show full hashes

# Recurrent functions ####

ProjectPath <- function(path = rstudioapi::getActiveProject()){
  path <- rstudioapi::selectDirectory(caption="Select the raw file directory",path=path)
  return(glue(path,"/"))
}

ProjectFile <- function(path = rstudioapi::getActiveProject()){
  path <- rstudioapi::selectFile(caption="Select the raw file directory",path=path)
  return(path)
}

SaveObject <- function(object, name, path = "~/datbuyer/dashbuyer/dataframes/") {
  dir.create(path, showWarnings = F)
  saveRDS(object, glue("{path}{name}.RDS"))
  print(glue("{name}.RDS saved at {path}"))
}

LoadObject <- function(path = ProjectFile(), separator = ",", save = TRUE) {
  file <- basename(path)
  extension <- str_extract(file, pattern = "\\.[A-z]+$")
  if (extension == ".csv") {
    object <- read.csv(path, sep = separator)
    if (save) {
      name <- str_extract(file, ".*(?=\\.[A-z]+$)")
      SaveObject(object, name)
    }
  } else if (extension == ".RDS") {
    object <- readRDS(path)
    return(object)
  } else {
    stop(glue("File format is not valid. Use either .csv or .RDS files instead."))
  }
}

# Data transformations ####

removeDuplicates <- function(cycles) {
  cycles <- cycles %>% select(Client_id, Hash, store, first_timestamp, last_timestamp, duration) %>% 
    mutate(first_timestamp = as.character(first_timestamp),
           last_timestamp = as.character(last_timestamp)) %>%
    distinct() %>% 
    mutate(first_timestamp = as.factor(first_timestamp),
           last_timestamp = as.factor(last_timestamp)) %>% 
    mutate(duration = round(duration / 60, 2))
  return(cycles)
}

# Computations ####

clients_nobuying <- function(tickets, cycles, clients_list) {
  n_clients <- 0
  for (client in clients_list) {
    last_ticket <- tickets %>%
      filter(Client_id == client) %>% 
      mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
      pull(datetime) %>% last()
    
    n_times_nobuying <- cycles %>% 
      filter(Client_id == client) %>%
      mutate(first_timestamp = as.POSIXct(first_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
      filter(first_timestamp > last_ticket) %>% nrow()
    
    if (n_times_nobuying > 0) n_clients <- n_clients + 1
  }
  
  return(n_clients/length(clients_list)*100)
}

all_clients_nobuying <- function(store_id, tickets, cycles) {
  
  tickets <- tickets %>% 
    filter(store == store_id)
  cycles <- cycles %>% 
    filter(store == store_id)
  clients_list <- cycles %>% pull(Client_id) %>% unique()
  
  n_clients <- 0
  for (client in clients_list) {
    last_ticket <- tickets %>%
      filter(Client_id == client) %>% 
      mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
      pull(datetime) %>% last()
    
    n_times_nobuying <- cycles %>% 
      filter(Client_id == client) %>%
      mutate(first_timestamp = as.POSIXct(first_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
      filter(first_timestamp > last_ticket) %>% nrow()
    
    if (n_times_nobuying > 0) n_clients <- n_clients + 1
  }
  
  return(n_clients/length(clients_list)*100)
}

# Tourist or resident




# Plots ####

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

TicketsPlot <- function(tickets_data) {
  # Extract YEAR-MONTH only.
  tickets_data <- tickets_data %>%
    mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
    mutate(year_month = format(datetime, "%Y-%m"))
  # Save first and last ticket dates.
  last_date <- tickets_data %>% pull(datetime) %>% last()
  first_date <- tickets_data %>% pull(datetime) %>% first()
  n_months <- elapsed_months(last_date, first_date)
  # Count
  tickets_data <- tickets_data %>% 
    group_by(year_month) %>% 
    summarise(hits = n())

  spaced_months <- data.frame(
    seq_months = seq(as.POSIXct(first_date), by = "month", length.out = n_months+1)
  ) %>% 
    mutate(year_month = format(seq_months, "%Y-%m")) %>% 
    left_join(tickets_data) %>% 
    mutate(hits = replace_na(hits, 0),
           th_month = format(seq_months, "%m") %>% as.numeric(),
           name_month = format(seq_months, "%b") %>% toupper()) %>% 
    mutate(name_month = fct_reorder(name_month, th_month))
  
  tickets_plot <- ggplotly(ggplot(spaced_months, aes(x = year_month, y = hits, fill = name_month)) +
                  geom_col(aes(text = paste0(hits, " purchase(s)",
                                             "\non ", year_month, " (", name_month,")"))) + 
                  theme(axis.text.x = element_text(angle = 65, vjust = 0.6, colour = "#28334AFF"),
                        plot.title = element_text(hjust = 0.5)) +
                  # ggtitle(glue("Purchasing history of client")) +
                  ggtitle("") +
                  xlab("") + 
                  ylab("Purchases") + 
                  guides(fill=guide_legend(title="Month"))
                , tooltip = c("text")) %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  
  return(tickets_plot)
}

CyclesPlot <- function(cycles_data) {
  # Extract YEAR-MONTH only.
  cycles_data <- cycles_data %>%
    mutate(first_timestamp = as.POSIXct(first_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
    mutate(year_month = format(first_timestamp, "%Y-%m"))
  # Save first and last ticket dates.
  last_date <- cycles_data %>% pull(first_timestamp) %>% last()
  first_date <- cycles_data %>% pull(first_timestamp) %>% first()
  n_months <- elapsed_months(last_date, first_date)
  # Count
  cycles_data <- cycles_data %>% 
    group_by(year_month) %>% 
    summarise(hits = n())
  
  spaced_months <- data.frame(
    seq_months = seq(as.POSIXct(first_date), by = "month", length.out = n_months+1)
  ) %>% 
    mutate(year_month = format(seq_months, "%Y-%m")) %>% 
    left_join(cycles_data) %>% 
    mutate(hits = replace_na(hits, 0),
           th_month = format(seq_months, "%m") %>% as.numeric(),
           name_month = format(seq_months, "%b") %>% toupper()) %>% 
    mutate(name_month = fct_reorder(name_month, th_month))
  
  cycles_plot <- ggplotly(ggplot(spaced_months, aes(x = year_month, y = hits, fill = name_month)) +
                             geom_col(aes(text = paste0(hits, " access(es)",
                                                        "\non ", year_month, " (", name_month,")"))) + 
                             theme(axis.text.x = element_text(angle = 65, vjust = 0.6, colour = "#28334AFF"),
                                   plot.title = element_text(hjust = 0.5)) +
                             # ggtitle(glue("Purchasing history of client")) +
                             ggtitle("") +
                             xlab("") + 
                             ylab("Acceses") + 
                             guides(fill=guide_legend(title="Month"))
                           , tooltip = c("text")) %>% layout(legend = list(orientation = "h", x = 0, y = 1.15))
  
  return(cycles_plot)
}


