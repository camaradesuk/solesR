#' Tag studies by risk of bias
#'
#' @import parallel
#' @import reticulate
#' @import readr
#' @param con connection to db
#' @param max_file_size maximum file size in bites
#' @param py_path path to python risk of bias tool e.g. "/opt/sharedFolder/rob/rob-app/"
#' @param num_cores number of cores to use for parallelisation
#' @export

rob_tag <- function(con, max_file_size = 500000, num_cores = 10, py_path=""){

  # check if table exists
  if (!dbExistsTable(con, "rob_tag")) {
    dbExecute(con, "CREATE TABLE rob_tag (doi TEXT, blind_prob REAL, exclusion_prob REAL, interest_prob REAL, random_prob REAL, welfare_prob REAL, is_blind TEXT, is_exclusion TEXT, is_interest TEXT, is_random TEXT, is_welfare TEXT)")
    message("Created rob_tag table.")
  }

  # get list of citations tagged with rob already
  tagged_already <- dbReadTable(con, "rob_tag")

  # get list of citations with pdfs - to tag
  # remove those tagged already
  pdfs_checked <- dbReadTable(con, "full_texts")

  # get list of smaller files
  all_files <- list.files(path="full_texts/", pattern=".txt", full.names= T)
  all_files <- gsub("\\/\\/", "/", all_files)

  # filter files based on size
  all_files <- all_files[file.info(all_files)$size < max_file_size]
  all_files <- all_files[file.info(all_files)$size > 10]

  dir <- getwd()

  # get citations to tag
  citations_to_tag <- pdfs_checked %>%
    filter(status == "found") %>%
    rename(id = doi) %>%
    filter(!id %in% tagged_already$doi) %>%
    select(path,id) %>%
    filter(path %in% all_files) %>%
    mutate(path = paste0(dir, "/", path))

  message(paste0(length(citations_to_tag$id), " citations left to tag (at specified file size!)"))

  if(length(citations_to_tag$id) > 350){

    # select sample to run RoB on
    citations_to_tag_sample <- citations_to_tag[1:350,]
    message(paste0(length(citations_to_tag_sample$id), " citations ready to tag (at specified file size!)"))

  } else {

    citations_to_tag_sample <- citations_to_tag
    message(paste0(length(citations_to_tag_sample$id), " citations ready to tag (at specified file size!)"))
  }

  # Split the dataframe into equally sized chunks
  num_chunks <- num_cores  # Adjust the number of chunks as per your requirement
  chunk_size <- ceiling(nrow(citations_to_tag_sample) / num_chunks)
  df_chunks <- split(citations_to_tag_sample, rep(1:num_chunks, each = chunk_size, length.out = nrow(citations_to_tag_sample)))

  # Export each chunk as a CSV file
  for (i in seq_along(df_chunks)) {
    chunk <- df_chunks[[i]]
    file_name <- paste0("to_tag_rob_", i, ".csv")
    write.csv(chunk, file = file_name, row.names = FALSE)
  }

  # Connect to Python
  # Run virtual python environment
  reticulate::use_virtualenv(py_path)
  py_config()

  # Create a cluster using the specified number of cores
  cl <- makeCluster(num_cores)


  # Specify files for python (use full path)
  files_to_tag <- list.files(dir, pattern="to_tag_rob_..?.csv", full.names = T)
  files_to_tag <- as.vector(files_to_tag)

  # Define the function to execute the command
  run_command <- function(file) {
    command <- paste0("python ", py_path, "/rob.py -p ", file)
    system(command)
    message("running...")
  }

  # Export necessary variables and functions to the cluster
  #parallel::clusterExport(cl, c("files_to_tag", "run_command"))

  # Run the command in parallel for each file
  results <- parLapply(cl, files_to_tag, run_command)

  # Stop the cluster
  stopCluster(cl)

  # read in newly tagged citations (output results file is in same folder as input)
  tagged_files <- list.files(path = dir, pattern = "to_tag_rob_..?_output", full.names = TRUE)
  tagged_citations <- readr::read_csv(tagged_files)
  tagged_citations <- tagged_citations[, -1]

  # remove irrelevant columns and format for db
  tagged_citations <- tagged_citations %>%
    select(-txt_path) %>%
    rename(doi = id) %>%
    mutate(is_blind = ifelse(blind > 0.5, "reported", "not reported"),
           is_random = ifelse(random  > 0.5, "reported", "not reported"),
           is_exclusion = ifelse(exclusion  > 0.5, "reported", "not reported"),
           is_welfare = ifelse(welfare  > 0.5, "reported", "not reported"),
           is_interest = ifelse(interest > 0.5, "reported", "not reported")) %>%
    mutate(is_blind = ifelse(blind > 998, "unknown", is_blind),
           is_random = ifelse(random  > 998, "unknown", is_random),
           is_exclusion = ifelse(exclusion  > 998, "unknown", is_exclusion),
           is_welfare = ifelse(welfare  > 998, "unknown", is_welfare),
           is_interest = ifelse(interest > 998, "unknown", is_interest)) %>%
    rename(blind_prob = blind,
           interest_prob = interest,
           welfare_prob = welfare,
           random_prob = random,
           exclusion_prob = exclusion)

  message(paste0(length(tagged_citations$doi), " citations tagged with RoB reporting!"))

  file.remove(files_to_tag)
  file.remove(tagged_files)

  dbWriteTable(con, "rob_tag", tagged_citations, append=TRUE)
}

