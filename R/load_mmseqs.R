#' Load MMseqs
#'
#' This function loads MMseqs data from a file, applies some filtering and transformations, and returns a data frame.
#'
#' @param file The file to load.
#' @param frag_thr The threshold for total_fragments.
#' @param sup_thr The threshold for support.
#' @param tax_ranks The taxonomy ranks to include.
#' @param include_name Whether to include the sample name in the output.
#' @return A data frame.
#' @importFrom stringr str_split
#' @importFrom readr read_delim
#' @importFrom dplyr mutate filter group_by summarise inner_join select arrange




load_mmseqs <- function(file,
                        frag_thr = 3,
                        sup_thr = 0.7,
                        tax_ranks = taxonomy_ranks,
                        include_name = FALSE) {
  # Include a check that the given file exists
  if (!file.exists(file)) {
    stop("File does not exist!")
  }

  # Initial loading of file and setting up the right column names and taxonomy
  main.df <-
    readr::read_tsv(file,
                    col_names = c("query_acc","NCBI_id","rank","name",
                                  "total_fragments","assigned_fragments",
                                  "agreement","support","taxonomy","whole_tax")) %>%
  # Fixing the lca data frame to be a proper data frame
    dplyr::filter(total_fragments >= frag_thr & support >= sup_thr & !rank %in% c("no rank","clade")) %>%
    dplyr::group_by(taxonomy) %>%
    dplyr::mutate(rank = gsub("superkingdom","kingdom",rank)) %>%
    dplyr::summarise(counts = n()) %>%
    dplyr::inner_join(lca.df %>% dplyr::filter(rank %in% tax_ranks) %>%
                        dplyr::select(name,rank,taxonomy,NCBI_id) %>%
                        unique(), by = "taxonomy") %>%
    dplyr::mutate(rank = factor(rank,levels = tax_ranks)) %>%
    dplyr::arrange(rank)

  # In case the sample name should be included
  if (include_name == TRUE) {
    # Extracts the sample name from the file name (it is always the same by default)
    sample_name <- gsub("_lca.tsv","",basename(file))
    main.df <- dplyr::mutate(main.df,sample=sample_name)
  }
  return(main.df)
}
