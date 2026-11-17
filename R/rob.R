#' Tag studies by risk of bias
#'
#' Changes from the original are marked with "CHANGE:" comments. In summary:
#'   - Python runs with a thread limit, so the jobs don't overload the server
#'     (max_threads / threads_per_job).
#'   - Papers can be re-tagged (dois / n_retag / seed) with results written to
#'     a CSV instead of the database (output_csv).
#'   - Each paper's file path is checked directly, and skipped papers are
#'     reported with the reason.
#'   - Failures are reported instead of passing silently.
#'   - Temporary files go in a private folder that is always cleaned up.
#'
#' @import parallel
#' @import reticulate
#' @import readr
#' @import dplyr
#' @param con connection to db
#' @param max_file_size maximum file size in bytes
#' @param num_cores maximum number of python jobs to run at once (each job
#'   loads all the models, so this also limits memory use)
#' @param py_path path to the risk of bias python script,
#'   e.g. "/opt/sharedFolder/rob/rob-app/rob_211026.py"
#'   (a folder containing rob.py also works)
#' @param venv python virtual environment to run the script in. Defaults to
#'   the folder the script is in, e.g. "/opt/sharedFolder/rob/rob-app/"
#' @param batch_size maximum number of citations to tag per call
#' @param threads_per_job CPU threads each python job may use (default 1), so
#'   the run uses num_cores * threads_per_job CPU cores in total. Capping
#'   threads changes probabilities around the 6th decimal place compared with
#'   the original (labels are unaffected). NULL leaves threads to PyTorch, as
#'   the original did: identical probabilities, but each job then tries to use
#'   every core on the machine.
#' @param dois optional character vector of DOIs to (re-)tag, e.g. df$doi.
#'   These are tagged whether or not they are already in rob_tag, batch_size
#'   is ignored, and results are written to output_csv, never to the database.
#' @param output_csv optional file path. If given, results are written to this
#'   CSV instead of the database. When dois is used and output_csv is not
#'   given, a timestamped file "rob_retag_YYYYMMDD_HHMMSS.csv" is written to
#'   the working directory.
#' @param n_retag optional number of papers to re-tag, picked at random from
#'   `dois`, or from all papers already in rob_tag if `dois` is not given.
#'   Only papers that can be tagged (full text found, within max_file_size)
#'   are sampled, so you get n_retag results if that many are available.
#' @param seed random seed for the n_retag sample, so the same papers can be
#'   picked again. NULL gives a different sample each time.
#' @param max_threads optional total number of CPU threads the app may use,
#'   e.g. 24 on a 48-core server. The function picks the number of jobs (up to
#'   num_cores) that uses as much of the budget as possible, preferring more
#'   jobs, and gives every job the same number of threads,
#'   e.g. max_threads = 24, num_cores = 10 gives 8 jobs x 3 threads. Overrides
#'   threads_per_job. Scores can differ around the 6th decimal place between
#'   runs that end up with a different number of threads per job.
#' @return the tagged citations, invisibly
#' @export

rob_tag <- function(con, max_file_size = 500000, num_cores = 10, py_path = "",
                    venv = NULL, batch_size = 350, threads_per_job = 1,
                    dois = NULL, output_csv = NULL, n_retag = NULL, seed = NULL,
                    max_threads = NULL) {
  
  # CHANGE: new arguments venv, batch_size, threads_per_job, dois, output_csv,
  # n_retag, seed and max_threads (documented above). The original had only
  # con, max_file_size, num_cores and py_path, which work as before.
  
  # CHANGE: new. Check max_threads before doing any work.
  if (!is.null(max_threads) && (length(max_threads) != 1 || is.na(max_threads) || max_threads < 1)) {
    stop("`max_threads` must be a single number of 1 or more.")
  }
  
  # CHANGE: py_path can now be the script itself (e.g. .../rob_211026.py) as
  # well as a folder containing rob.py. The original always added "/rob.py",
  # which gave ".../rob_211026.py/rob.py" for a script path. Stops early if
  # the script doesn't exist.
  # risk of bias script
  rob_script <- if (grepl("\\.py$", py_path)) py_path else file.path(py_path, "rob.py")
  if (!file.exists(rob_script)) stop("Risk of bias script not found: ", rob_script)
  
  # Connect to Python
  # Run virtual python environment (the script's folder unless venv is given)
  # CHANGE: same approach as the original (use_virtualenv + py_config), but:
  #  - the virtualenv defaults to the script's folder, since py_path is now
  #    the script rather than the folder;
  #  - required = TRUE stops with a clear error if it isn't a virtualenv,
  #    instead of silently using another python;
  #  - this is done before any files are written, so a problem stops it early;
  #  - the python it finds is used directly to run the jobs (see below),
  #    rather than relying on "python" being on the PATH.
  if (is.null(venv)) venv <- dirname(rob_script)
  reticulate::use_virtualenv(venv, required = TRUE)
  python_bin <- reticulate::py_config()$python
  message("Using python: ", python_bin)
  
  # CHANGE: new section. Re-tag mode: tag specific DOIs (dois), or a random
  # sample of them (n_retag), whether or not they are already tagged.
  # Results go to a CSV, never the database.
  if (!is.null(n_retag) && (length(n_retag) != 1 || is.na(n_retag) || n_retag < 1)) {
    stop("`n_retag` must be a single number of 1 or more.")
  }
  retag <- !is.null(dois) || !is.null(n_retag)
  if (retag) {
    if (is.null(dois)) {
      # sample from everything already tagged
      dois <- dbGetQuery(con, "SELECT DISTINCT doi FROM rob_tag")$doi
      if (length(dois) == 0) stop("rob_tag is empty, so there is nothing to re-tag.")
    }
    dois <- unique(as.character(dois[!is.na(dois)]))
    if (length(dois) == 0) stop("`dois` is empty.")
    if (is.null(output_csv)) {
      output_csv <- paste0("rob_retag_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    }
  }
  
  # check if table exists
  if (!dbExistsTable(con, "rob_tag")) {
    dbExecute(con, "CREATE TABLE rob_tag (doi TEXT, blind_prob REAL, exclusion_prob REAL, interest_prob REAL, random_prob REAL, welfare_prob REAL, is_blind TEXT, is_exclusion TEXT, is_interest TEXT, is_random TEXT, is_welfare TEXT)")
    message("Created rob_tag table.")
  }
  
  # citations already tagged (only the doi column is needed)
  # CHANGE: reads only the doi column; the original read the whole rob_tag table.
  tagged_already <- dbGetQuery(con, "SELECT doi FROM rob_tag")
  
  # citations with full texts
  pdfs_checked <- dbReadTable(con, "full_texts")
  
  dir <- getwd()
  
  # CHANGE: the original listed the .txt files in "full_texts/" and kept papers
  # whose stored path matched one of them exactly as text, so any path written
  # differently (absolute, "./...", "//", another folder) was silently skipped.
  # Now each found paper's stored path is checked directly, and its status
  # is recorded: "ok", "not txt", "missing", "empty" (10 bytes or less) or
  # "too big" (max_file_size or more). The size limits are the same as before.
  # check each found paper's stored path directly: relative paths are taken
  # from the working directory, absolute paths are used as they are
  found <- pdfs_checked %>%
    filter(status == "found") %>%
    rename(id = doi) %>%
    select(path, id)
  
  is_abs   <- grepl("^(/|~|[A-Za-z]:)", found$path)
  rel      <- sub("^\\./", "", found$path)
  full     <- ifelse(is_abs, path.expand(found$path), file.path(dir, rel))
  found$path <- gsub("/{2,}", "/", full)
  size     <- file.info(found$path)$size
  found$file_status <- ifelse(!grepl("\\.txt$", found$path, ignore.case = TRUE), "not txt",
                              ifelse(is.na(found$path) | is.na(size), "missing",
                                     ifelse(size <= 10, "empty",
                                            ifelse(size >= max_file_size, "too big", "ok"))))
  found$size <- size
  
  if (retag) {
    # CHANGE: new (re-tag mode).
    # tag exactly the requested DOIs, already tagged or not
    # if a DOI has several full_texts rows (e.g. a .pdf and a .txt), use a usable one
    requested <- found %>%
      filter(id %in% dois) %>%
      arrange(id, file_status != "ok") %>%
      distinct(id, .keep_all = TRUE)
    
    # report each reason a requested DOI can't be tagged (first few shown)
    show <- function(x, n = 10) {
      paste(c(head(x, n), if (length(x) > n) paste("... and", length(x) - n, "more")),
            collapse = "\n")
    }
    no_text <- setdiff(dois, requested$id)
    if (length(no_text) > 0) {
      warning(length(no_text), " DOI(s) skipped: no 'found' full text in the full_texts table:\n",
              show(no_text), call. = FALSE)
    }
    not_txt <- filter(requested, file_status == "not txt")
    if (nrow(not_txt) > 0) {
      warning(nrow(not_txt), " DOI(s) skipped: full text path is not a .txt file, e.g.:\n",
              show(paste0(not_txt$id, "  ->  ", not_txt$path), 5), call. = FALSE)
    }
    missing <- filter(requested, file_status == "missing")
    if (nrow(missing) > 0) {
      warning(nrow(missing), " DOI(s) skipped: text file not found. Working directory is ", dir,
              "; looked for e.g.:\n", show(paste0(missing$id, "  ->  ", missing$path), 5),
              call. = FALSE)
    }
    empty <- filter(requested, file_status == "empty")
    if (nrow(empty) > 0) {
      warning(nrow(empty), " DOI(s) skipped: text file empty (10 bytes or less):\n",
              show(empty$id), call. = FALSE)
    }
    big <- filter(requested, file_status == "too big")
    if (nrow(big) > 0) {
      warning(nrow(big), " DOI(s) skipped: text file larger than max_file_size (",
              format(max_file_size, scientific = FALSE), " bytes; largest is ",
              format(max(big$size), scientific = FALSE), "):\n", show(big$id), call. = FALSE)
    }
    
    citations_to_tag_sample <- requested %>%
      filter(file_status == "ok") %>%
      select(path, id)
    
    # random sample of n_retag (reproducible with seed)
    if (!is.null(n_retag)) {
      available <- nrow(citations_to_tag_sample)
      if (n_retag > available) {
        warning("n_retag is ", n_retag, " but only ", available,
                " papers can be re-tagged; tagging all of them.", call. = FALSE)
      } else {
        if (!is.null(seed)) set.seed(seed)
        citations_to_tag_sample <- citations_to_tag_sample[sample.int(available, n_retag), ]
      }
      message(nrow(citations_to_tag_sample), " citations sampled to re-tag (from ",
              available, " available)")
    } else {
      message(nrow(citations_to_tag_sample), " of ", length(dois),
              " requested citations ready to re-tag")
    }
  } else {
    # citations not yet tagged (as in the original)
    citations_to_tag <- found %>%
      filter(!id %in% tagged_already$doi) %>%
      filter(file_status == "ok") %>%
      select(path, id)
    
    message(nrow(citations_to_tag), " citations left to tag (at specified file size!)")
    
    # CHANGE: the batch size (350 in the original, hard-coded) is now the
    # batch_size argument, defaulting to 350.
    citations_to_tag_sample <- head(citations_to_tag, batch_size)
    message(nrow(citations_to_tag_sample), " citations ready to tag (at specified file size!)")
  }
  
  # CHANGE: new. Stop cleanly if there is nothing to tag (the original carried
  # on and failed later).
  if (nrow(citations_to_tag_sample) == 0) return(invisible(NULL))
  
  # CHANGE: chunk CSVs, logs and outputs go in a private temporary folder that
  # is deleted however the function exits. The original wrote them to the
  # working directory, removed them only at the very end, and found them
  # again by file name pattern, so leftovers from a crashed run could be
  # picked up by the next run.
  # private working folder, removed however the function exits
  work_dir <- tempfile("rob_tag_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  
  # CHANGE: number of jobs and threads.
  #  - never more jobs than papers (the original always made num_cores chunks);
  #  - with max_threads, the thread budget is split between jobs: the number
  #    of jobs (up to num_cores) that uses most of the budget is chosen, and
  #    every job gets the same number of threads;
  #  - papers are dealt out to the jobs in turn (1, 2, 3, ..., 1, 2, 3, ...)
  #    instead of in consecutive blocks, so the chunks are evenly sized.
  # split into one chunk per job. With max_threads, the thread budget decides
  # the number of jobs (up to num_cores) and threads are shared evenly
  num_chunks <- min(num_cores, nrow(citations_to_tag_sample))
  if (!is.null(max_threads)) {
    # pick the number of jobs that uses most of the budget (ties: more jobs)
    jobs <- seq_len(min(num_chunks, max_threads))
    used <- jobs * floor(max_threads / jobs)
    num_chunks      <- max(jobs[used == max(used)])
    threads_per_job <- floor(max_threads / num_chunks)
  }
  message("Running ", num_chunks, " job(s) x ",
          if (is.null(threads_per_job)) "PyTorch default threads" else
            paste0(threads_per_job, " thread(s) = ", num_chunks * threads_per_job, " threads"))
  chunk_ids <- rep_len(seq_len(num_chunks), nrow(citations_to_tag_sample))
  df_chunks <- split(citations_to_tag_sample, chunk_ids)
  
  files_to_tag <- file.path(work_dir, paste0("to_tag_rob_", seq_along(df_chunks), ".csv"))
  for (i in seq_along(df_chunks)) {
    write.csv(df_chunks[[i]], file = files_to_tag[i], row.names = FALSE)
  }
  
  # CHANGE: how the python jobs are run.
  #  - Thread limit: the original set no limit, so PyTorch gave each of the
  #    10 jobs about 24 threads (one per physical core), ~240 threads on a
  #    48-thread server, which overloaded it. Each job is now limited to
  #    threads_per_job threads (set via max_threads, or 1 by default).
  #    threads_per_job = NULL restores the original behaviour.
  #  - mclapply() replaces makeCluster() + parLapply(): lighter, no need to
  #    copy the function's data to separate R worker processes.
  #  - system2() runs the virtualenv's python directly, with arguments quoted
  #    so paths with spaces work, and each job's output goes to its own log
  #    file. The original's system() output and exit status were ignored.
  # run rob.py on each chunk in parallel, capping each job's threads so the
  # jobs don't overload the server (threads_per_job = NULL leaves it to PyTorch)
  thread_env <- if (is.null(threads_per_job)) character(0) else
    paste0(c("OMP_NUM_THREADS=", "MKL_NUM_THREADS=", "OPENBLAS_NUM_THREADS="), threads_per_job)
  exit_codes <- parallel::mclapply(files_to_tag, function(file) {
    log_file <- sub("\\.csv$", ".log", file)
    system2(python_bin, c(shQuote(rob_script), "-p", shQuote(file)),
            env = thread_env, stdout = log_file, stderr = log_file)
  }, mc.cores = num_chunks, mc.preschedule = FALSE)
  
  log_files <- sub("\\.csv$", ".log", files_to_tag)
  
  # CHANGE: new. Report papers that a model failed on. rob.py records them as
  # unknown (999) and prints a "FAILED" line; they are listed in one warning.
  # papers that errored inside the models (the item(s) affected are recorded as
  # "unknown"; each line says whether only welfare or the whole paper failed)
  failed_papers <- unlist(lapply(log_files[file.exists(log_files)], function(f) {
    grep("^FAILED ", readLines(f, warn = FALSE), value = TRUE)
  }))
  if (length(failed_papers) > 0) {
    warning(length(failed_papers), " paper(s) had errors; the affected items were recorded as unknown:\n",
            paste(failed_papers, collapse = "\n"), call. = FALSE)
  }
  
  # CHANGE: new. Report whole jobs that crashed (in the original these failed
  # silently and their papers were lost). The job's full log is copied to the
  # working directory, as the temporary folder is deleted.
  # whole chunks that crashed: keep their full logs, since work_dir is deleted
  failed <- which(unlist(exit_codes) != 0)
  for (i in failed) {
    kept_log <- file.path(dir, paste0("rob_tag_failed_chunk_", i, ".log"))
    file.copy(log_files[i], kept_log, overwrite = TRUE)
    warning("rob.py failed on chunk ", i, "; full log saved to ", kept_log, "\nLast lines:\n",
            paste(tail(readLines(log_files[i], warn = FALSE), 3), collapse = "\n"), call. = FALSE)
  }
  
  # CHANGE: output files are taken from the chunk names (not found by pattern),
  # and if none were produced the function stops with a warning.
  # read in newly tagged citations
  tagged_files <- sub("\\.csv$", "_output.csv", files_to_tag)
  tagged_files <- tagged_files[file.exists(tagged_files)]
  if (length(tagged_files) == 0) {
    warning("No citations were tagged.")
    return(invisible(NULL))
  }
  
  # CHANGE: show_col_types / name_repair silence readr's column messages
  # (e.g. "New names: `` -> `...1`"); the data read is the same.
  tagged_citations <- readr::read_csv(tagged_files, show_col_types = FALSE,
                                      name_repair = "unique_quiet")
  tagged_citations <- tagged_citations[, -1]
  
  # CHANGE: the original set each label with two rounds of ifelse() (first
  # reported / not reported, then unknown for 999). This helper does both in
  # one step; the labels produced are the same.
  label <- function(p) {
    ifelse(p > 998, "unknown", ifelse(p > 0.5, "reported", "not reported"))
  }
  
  tagged_citations <- tagged_citations %>%
    select(-txt_path) %>%
    rename(doi = id) %>%
    mutate(is_blind     = label(blind),
           is_random    = label(random),
           is_exclusion = label(exclusion),
           is_welfare   = label(welfare),
           is_interest  = label(interest)) %>%
    rename(blind_prob     = blind,
           interest_prob  = interest,
           welfare_prob   = welfare,
           random_prob    = random,
           exclusion_prob = exclusion)
  
  message(nrow(tagged_citations), " citations tagged with RoB reporting!")
  
  # CHANGE: results go to output_csv if given (always in re-tag mode),
  # otherwise to the database as in the original. The original's
  # file.remove() calls are replaced by the on.exit() clean-up above.
  if (is.null(output_csv)) {
    dbWriteTable(con, "rob_tag", tagged_citations, append = TRUE)
  } else {
    readr::write_csv(tagged_citations, output_csv)
    message("Results written to ", normalizePath(output_csv), " (database not changed)")
  }
  
  # CHANGE: new. Returns the results (invisibly) so they can be used directly,
  # e.g. res <- rob_tag(...).
  invisible(tagged_citations)
}