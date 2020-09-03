
rds_to_csv <- function(trap_data){
raw_bead <- trap_data$grouped[[1]]$bead
processed_bead <- trap_data$processed[[1]]$bead

d <- data.frame(project = trap_data$project,
                conditions = trap_data$conditions,
                date = trap_data$date,
                obs = trap_data$obs,
                raw_bead = raw_bead,
                processed_bead = processed_bead,
                processor = trap_data$processed_how,
                include = trap_data$include,
                mv2nm = trap_data$mv2nm,
                nm2pn = trap_data$nm2pn,
                analyzer = trap_data$analyzer, 
                report = trap_data$report,
                quality_control = trap_data$quality_control)


path <- file.path("~/lasertrapr", 
                  unique(d$project),
                  unique(d$conditions),
                  unique(d$date),
                  unique(d$obs), 
                  'trap-data.csv')

vroom::vroom_write(d, path, delim = ',')
}

# trap_data %<>% 
#   nest(data = c(raw_bead, processed_bead)) %>% 
#   dplyr::filter(include == T)
