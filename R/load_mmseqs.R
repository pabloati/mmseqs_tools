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
                        tax_ranks = c("kingdom","phylum","class","order","family","genus","species"),
                        include_name = FALSE) {
  sample_name <- stringr::str_split(basename(file),"_",simplify = TRUE)[1]

  # Initial loading of file and setting up the right column names and taxonomy
  lca.df <- readr::read_delim(file,header=FALSE) %>%
    dplyr::mutate(V3 = gsub("superkingdom","kingdom",V3))
  colnames(lca.df) <- c("query_acc","NCBI_id","rank","name","total_fragments","assigned_fragments","agreement","support","taxonomy","whole_tax")

  # Fixing the lca data frame to be a proper data frame
  main.df <- lca.df %>%
    dplyr::filter(total_fragments >= frag_thr & support >= sup_thr & !rank %in% c("no rank","clade")) %>%
    dplyr::group_by(taxonomy) %>%
    dplyr::summarise(counts = n()) %>%
    dplyr::inner_join(lca.df %>% dplyr::filter(rank %in% tax_ranks) %>% dplyr::select(name,rank,taxonomy,NCBI_id) %>% unique(), by = "taxonomy") %>%
    dplyr::mutate(rank = factor(rank,levels = tax_ranks)) %>%
    dplyr::arrange(rank)

  # In case the sample name should be included
  if (include_name == TRUE) {
    main.df <- dplyr::mutate(main.df,sample=sample_name)
  }
  return(main.df)
}
