# download audit files ----------------------------------------------------

download_audit_files <- function(df, uuid_column = "_uuid", audit_dir, usr, pass){
  if (!"httr" %in% installed.packages()) 
    stop("The package is httr is required!")
  if (is.na(audit_dir) || audit_dir == "") 
    stop("The path for storing audit files can't be empty!")
  if (is.na(usr) || usr == "") 
    stop("Username can't be empty!")
  if (is.na(pass) || pass == "") 
    stop("Password can't be empty!")
  
  # checking if the output directory is already available
  if (!dir.exists(audit_dir)) {
    dir.create(audit_dir)
    if (dir.exists(audit_dir)) {
      cat("Attention: The audit file directory was created in", audit_dir,"\n")
    }
  }
  
  # checking if creating output directory was successful
  if (!dir.exists(audit_dir))
    stop("download_audit_fils was not able to create the output directory!")
  # checking if uuid column exists in data set
  if (!uuid_column %in% names(df))
    stop("The column ", uuid_column, " is not available in data set.")
  # checking if column audit_URL exists in data set
  if (!uuid_column %in% names(df))
    stop("The column ", uuid_column, " is not available in data set.")
  if (!"audit_URL" %in% names(df))
    stop("Error: the column audit_URL is not available in data set.")
  
  # getting the list of uuids that are already downloaded
  available_audits <- dir(audit_dir)
  
  # excluding uuids that their audit files are already downloaded
  df <- df[!df[[uuid_column]] %in% available_audits,]
  
  audits_endpoint_link <- df[["audit_URL"]]
  names(audits_endpoint_link) <- df[[uuid_column]]
  audits_endpoint_link <- na.omit(audits_endpoint_link)
  
  if (length(audits_endpoint_link) > 0) {
    # iterating over each audit endpoint from data
    for (i in 1:length(audits_endpoint_link)) {
      uuid = names(audits_endpoint_link[i])
      endpoint_link_i <- audits_endpoint_link[i]
      cat("Downloading audit file for", uuid, "\n")
      
      # requesting data
      audit_file <- content(GET(endpoint_link_i,
                                authenticate(usr, pass),
                                timeout(1000),
                                progress()), "text", encoding = "UTF-8")
      
      if (!is.na(audit_file)) {
        if (length(audit_file) > 2) {
          dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
          write.csv(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F)
        }else if(!audit_file == "Attachment not found"){
          if (grepl("[eventnodestartend]", audit_file)) {
            dir.create(paste0(audit_dir, "/", uuid), showWarnings = F)
            write.table(audit_file, paste0(audit_dir, "/", uuid, "/audit.csv"), row.names = F, col.names = FALSE, quote = F)
          } else{
            cat("Error: Downloading audit was unsucessful!\n")
          }
        }
      } else{
        cat("Error: Downloading audit was unsucessful!\n")
      }
    }
  } else{
    cat("Attention: All audit files for given data set is downloaded!")
  }
}

# silhouette analysis based on gower distance between surveys -------------

# silhouette analysis based on gower distance between surveys
# METHOD: check for anomalies using the silhouette function. We assume the dataset is clustered using the 
# enumerator IDs as the cluster IDs and we calculate the silhouette for this clustering scenario. A 
# silhouette value close to 1 indicates that the entries of the cluster are very similar to each other and 
# very dissimilar from entries of other clusters. Thus, we need to raise a flag if the silhouette value gets 
# close to 1 for any of the clusters/enumerators.
# https://en.wikipedia.org/wiki/Silhouette_(clustering)
# https://dpmartin42.github.io/posts/r/cluster-mixed-types
# https://medium.com/@rumman1988/clustering-categorical-and-numerical-datatype-using-gower-distance-ab89b3aa90d9

calculateEnumeratorSimilarity <- function(data, input_df_survey, col_enum, col_admin){
  
  # helper function
  convertColTypes <- function(data, input_df_survey){
    # select_multiple: numeric or factor?
    col.types <- data.frame(column=colnames(data)) %>% 
      left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
      mutate(type.edited = case_when(type %in% c("integer", "decimal", "calculate") ~ "numeric",
                                     str_starts(type, "select_") ~ "factor",
                                     str_detect(column, "/") ~ "factor",
                                     TRUE ~ "text"))
    
    cols <- col.types[col.types$type.edited=="numeric", "column"]
    data[,cols] <- lapply(data[,cols], as.numeric)
    cols <- col.types[col.types$type.edited=="text", "column"]
    data[,cols] <- lapply(data[,cols], as.character)
    cols <- col.types[col.types$type.edited=="factor", "column"]
    data[,cols] <- lapply(data[,cols], as.factor)
    
    return(data)
  }
  
  # convert columns using the tool
  data <- convertColTypes(data, input_df_survey)
  # keep only relevant columns
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(select(input_df_survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("_uuid", "enumerator_id")) &
             !str_starts(column, "_other$") &
             !str_detect(column, "_specify$"))
  # convert character columns to factor and add enum.id
  data <- data[, all_of(cols$column)] %>% 
    mutate_if(is.character, factor) %>% 
    arrange(!!sym(col_enum)) %>%
    mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
  
  # calculate similarity (for enumerators who completed at least 5 surveys)
  res <- data %>% split(data[[col_admin]]) %>% 
    lapply(function(gov){
      df <- gov %>% 
        group_by(enum.id) %>% 
        mutate(n=n()) %>% 
        filter(n>=5) %>% 
        ungroup() %>% 
        select_if(function(x) any(!is.na(x)))
      
      if (length(unique(df$enum.id)) > 1){
        # calculate gower distance
        gower_dist <- daisy(select(df, -c(!!sym(col_enum), enum.id)), 
                            metric = "gower", warnBin = F, warnAsym = F, warnConst = F)
        # gower_mat <- as.matrix(gower_dist)
        # calculate silhouette
        si <- silhouette(df$enum.id, gower_dist)
        res.si <- summary(si)
        # create output
        r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
          left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
          left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
          select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% 
          arrange(-si)
        return(r)
      }
    })
  do.call(rbind, res)
}

# combine the log but keep the change type and already filled values --------

create_combined_log_keep_change_type <- function(list_of_log,
                                                 dataset_name = "checked_dataset") {
  ## log must be a list
  
  if (is.data.frame(list_of_log) | is.character(list_of_log)) {
    stop(glue::glue("list_of_log must be a list which should contain the logs."))
  }
  
  ## look for dataset name
  if (!is.null(dataset_name)) {
    if (!dataset_name %in% names(list_of_log)) {
      stop(glue::glue(dataset_name, " can not be found in the list_of_log."))
    }
  }
  
  if (is.null(dataset_name) & "checked_dataset" %in% names(list_of_log)) {
    warning(glue::glue("You have a checked_dataset element in the list_of_log even though you have set dataset_name to NULL. Please check the parameter."))
  }
  
  if (is.null(dataset_name) & !"checked_dataset" %in% names(list_of_log)) {
    message(glue::glue("No dataset name is provided. Assuming that the dataset does not exist in the list_of_log."))
  }
  
  output <- list()
  
  if (!is.null(dataset_name)) {
    output[["checked_dataset"]] <- list_of_log[[dataset_name]]
  }
  
  list_of_log_only <- list_of_log[names(list_of_log)[!names(list_of_log) %in% dataset_name]]
  
  list_of_log_only <- list_of_log_only %>%
    purrr::map(.f = ~ dplyr::mutate(., dplyr::across(
      .cols = tidyselect::everything(),
      .fns = ~ format(., scientific = F, justify = "none", trim = T)
    )))
  
  print(names(list_of_log) |> glue::glue_collapse(", ") %>% glue::glue("List of element to combine- ", .))
  
  output[["cleaning_log"]] <- dplyr::bind_rows(list_of_log_only)# |>
  # dplyr::mutate(
  #   change_type = NA_character_,
  #   new_value = NA_character_
  # )
  
  if(is.null(output[["cleaning_log"]][["check_binding"]])) {
    output[["cleaning_log"]] <- output[["cleaning_log"]] |>
      dplyr::mutate(
        check_binding = paste(question, uuid, sep = " ~/~ ")
      )
  } else {
    output[["cleaning_log"]] <- output[["cleaning_log"]] |>
      dplyr::mutate(
        check_binding = dplyr::case_when(is.na(check_binding) ~ paste(question, uuid, sep = " ~/~ "),
                                         TRUE ~  check_binding)
      )
    
  }
  
  output
}