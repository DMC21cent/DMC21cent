#' A Rate plot comparing Adverse Events (preferred terms) of interest by treatment
#'
#' @param adae Adverse Event dataset
#' @param adsl Subject Level dataset
#' @param title A string shows the main title of plot.
#' @param subtitle A string shows the subtitle of plot.
#' @param caption A string shows the caption of plot.
#' @param reord_type_rateplot A string shows how to reorder the `AEDECOD` on the plot.
#'   Options are `diff` (arrange based on difference between treatment and placebo),
#'   `trt` (arranged based on the treatment value), and
#'   `placebo` (arranged based on the Placebo value).
#'
#' @import ggplot2
#' @keywords internal
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#'  ae_rate_plot(adae = adae,
#'                 adsl = adsl)
#' }
ae_rate_plot <- function(adae, adsl, reord_type_rateplot, title, subtitle, caption) {

  big_n <- adsl %>%
    dplyr::group_by(TRT01AN) %>%
    #  summarise(n = n()) %>%
    dplyr::tally()

  patients <-
    adsl %>%
    dplyr::filter(SAFFL == "Y") %>%
    dplyr::select(USUBJID)

  ## get event spine i.e.
  ## if one arm dosen't report event, set to zero.
  event_spine <- adae %>%
    dplyr::distinct(AEDECOD)

  events <- adae %>%
    dplyr::group_by(TRTAN, AEDECOD) %>%
    dplyr::summarise(x = dplyr::n()) %>%
    dplyr::mutate(TRT01AN = TRTAN) %>%
    dplyr::ungroup() %>%
    dplyr::select(-TRTAN)


  events2 <-
    events %>%
    tidyr::pivot_wider(names_from = TRT01AN , values_from = x, values_fill = 0)

  events3 <-
    events2 %>%
    tidyr::pivot_longer(-AEDECOD, names_to = "TRT01AN", values_to = "x")



  big_n2 <-
    big_n %>%
    tidyr::pivot_wider(names_from = TRT01AN ,
                          values_from = n,
                          values_fill = 0,
                          names_prefix = "N_")

  events4 <-
    events3 %>%
    dplyr::mutate(TRT01AN = as.numeric(TRT01AN)) %>%
    dplyr::left_join(big_n)


  ############### TREATMENT estimates ####################
  dat <- events4 %>%
    dplyr::mutate(
      prev = x/n,
      perc = prev * 100)


  ###############3 TREATMENT comparisons ####################
  ## TODO calculate from a model###
  ### http://freerangestats.info/blog/2018/08/17/risk-ratios
  #### need to think about hazard ratio - -- set up a ADTTE
  #### how to handle multiple events per patient -- issue with
  #### crude incidence

  comp <-
    dat %>%
    dplyr::group_by(AEDECOD) %>%
    dplyr::summarise(ratio = (prev[TRT01AN==81] + 0.000001) / (prev[TRT01AN==0] + 0.000001),
              diff = prev[TRT01AN==81] - prev[TRT01AN==0],
              odds = (1 - prev[TRT01AN==81]) / (1 - prev[TRT01AN==0]))


  #############################################################

  dat2 <-
    dat %>%
    dplyr::rename(comparison = TRT01AN) %>%
    tidyr::pivot_longer(-c(AEDECOD, comparison), names_to = "statistic", values_to = "values")

  comp2 <-
    comp %>%
    dplyr::mutate(comparison = 0) %>%
    tidyr::pivot_longer(-c(AEDECOD, comparison), names_to = "statistic", values_to = "values")

  graph_data <- dplyr::bind_rows(dat2, comp2)

  graph_data2 <-
    graph_data %>%
    tidyr::pivot_wider(names_from = statistic, values_from = values)

  # graph_data2 %>%
  #   dplyr::group_by(AEDECOD) %>%
  #   dplyr::filter(perc > 0) %>%
  #   dplyr::ungroup()




  #graph_data2 %>% glimpse()


  graph_data2 <- graph_data2 %>%
    dplyr::mutate(AEDECOD = stringr::str_to_title(AEDECOD),
                  trt = dplyr::if_else(comparison == 0, "Placebo", "Xanomeline")) %>%
    dplyr::group_by(AEDECOD) %>%
    dplyr::filter(perc[comparison == 81] > 5) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AEDECOD = as.factor(AEDECOD))


  if(reord_type_rateplot == "diff"){
  AEDECOD_reord <- graph_data2%>%
    filter(!is.na(diff)) %>%
    dplyr::arrange(diff) %>%
    dplyr::pull(AEDECOD) %>%
    unique()

  graph_data2 <- graph_data2 %>%
    dplyr::mutate(AEDECOD = factor(AEDECOD, levels = AEDECOD_reord))

  } else if(reord_type_rateplot == "trt" ) {

    AEDECOD_reord <- graph_data2%>%
      filter(trt != "Placebo") %>%
      dplyr::arrange(prev) %>%
      dplyr::pull(AEDECOD) %>%
      unique()

    graph_data2 <- graph_data2 %>%
      dplyr::mutate(AEDECOD = factor(AEDECOD, levels = AEDECOD_reord))

  } else if(reord_type_rateplot == "placebo") {
    AEDECOD_reord <- graph_data2%>%
      filter(trt == "Placebo") %>%
      dplyr::arrange(prev) %>%
      dplyr::pull(AEDECOD) %>%
      unique()

    graph_data2 <- graph_data2 %>%
      dplyr::mutate(AEDECOD = factor(AEDECOD, levels = AEDECOD_reord))

  } else {
    stop("`reord_type_rateplot` should be happend based on either 'diff', 'trt', and 'placebo'. type.")
  }

  plot_02b <-
    graph_data2 %>%
    ggplot(aes(
      y = AEDECOD,
      x = prev,
      group = trt,
      colour = trt
    )) +
    geom_point(size = 3) +
    geom_line(aes(group = AEDECOD), size = 0.7, alpha = 0.25, colour = "black") +
    scale_x_continuous(breaks = c(0.1, 0.2, 0.3, 0.4, 0.5)) +
    guides(colour=guide_legend(title="Treatment group")) +
    scale_color_brewer(palette = 3) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      panel.grid.major.y = element_line(size = 0.3),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text.y = element_text(angle = 0),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    xlab("Adverse event incidence") +
    labs (title = title,
          subtitle = subtitle,
          caption = caption)


  return(plot_02b)

}
