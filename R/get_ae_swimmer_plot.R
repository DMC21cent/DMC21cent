#' A swimmer plot comparing Adverse Events (preferred terms) of interest by treatment
#'
#'
#' @param adae Adverse Event dataset
#' @param adsl Subject Level dataset
#' @param title A string shows the main title plot.
#' @param subtitle A string shows the subtitle plot.
#' @param caption A string shows the caption of plot.
#'
#' @import ggplot2
#' @return A ggplot object
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  ae_rate_plot(adae = adae,
#'                 adsl = adsl)
#' }
ae_swimmer_plot <- function(adae, adsl, title, subtitle, caption) {
  ## Select only those patients with one severe on treatment adverse event
  pts_sev <- adae %>%
    dplyr::filter(AESEV == "SEVERE" & TRTEMFL == "Y") %>%
    dplyr::select(USUBJID) %>%
    dplyr::distinct() %>%
    dplyr::left_join(adsl %>%
                       dplyr::select(USUBJID, TRTDUR, TRT01A)) %>%
    dplyr::arrange(TRTDUR) %>%
    dplyr::group_by(TRT01A) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::ungroup()


  ## Add all AEs from this subset of patients
  adae_sev <-
    pts_sev %>%
    dplyr::left_join(adae)


  ## select distinct AEs - there are duplicates
  ## index AE for presentation
  ## check if AE end date exists  - create variable for aligning text label
  ## put PT in to title case
  adae_sev <- adae_sev %>%
    dplyr::distinct_all() %>%
    dplyr::group_by(USUBJID, AEDECOD, ASTDY, AENDY ) %>%
    dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::arrange(ASTDY) %>%
    dplyr::mutate(incr = dplyr::row_number(),
           endd = dplyr::if_else(!is.na(AENDY), AENDY, TRTDUR)) %>%
    dplyr::mutate(AEDECOD = stringr::str_to_title(AEDECOD)) %>%
    dplyr::ungroup()


  adae_sev <-
    adae_sev %>%
    dplyr::arrange(USUBJID) %>%
    mutate(color = case_when(
      AESEV == "SEVERE" ~ "red",
      TRUE ~ "black"
    ))

  ##################################3
  ## Plot swimmer plot
  ## AE sequence + facets
  ## Plot only Active treatment
  ##################################


  plot_04 <-

    ## exposure indicator
    pts_sev %>%
    dplyr::filter(TRT01A != "Placebo") %>%
    ggplot() +
    geom_rect(aes(xmin = 0, xmax = TRTDUR, ymin = 0, ymax = 12),
              colour = "grey", alpha = 0.2) +

    geom_linerange(
      data = adae_sev %>%
        dplyr::filter(TRT01A != "Placebo"),
      aes(
        y = incr,
        x = ASTDY,
        xmin = ASTDY,
        xmax = endd,
        group = AEDECOD,
        colour = color
      ),
      size = 0.4
    ) +


  ## start date of adverse event
    geom_point(
      data = adae_sev %>%
        dplyr::filter(TRT01A != "Placebo"),
      aes(
        y = incr,
        x = ASTDY,
        color = color
      ),
      shape = "",
      size = 3
    ) +



    ## end date of AE
    geom_point(
      data = adae_sev %>%
        dplyr::filter(TRT01A != "Placebo"),
      aes(
        y = incr,
        x = endd +1,
        color = color
      ),
      shape = "|",
      size = 3
    ) +

    ## text label for AE - link to end date
    ggrepel::geom_text_repel(
    #geom_text(
      data = adae_sev %>%
        dplyr::filter(TRT01A != "Placebo") %>%
        dplyr::filter(AESER != "Y"),
      aes(
        y = incr+0.05,
        x = endd + 2,
        label = AEDECOD,
        group = AEDECOD,
        colour = color),
      size = 3,
      #position = position_dodge(width = 1),
      hjust=-0.1
      #  check_overlap = TRUE,
    ) +
    ggrepel::geom_text_repel(
      #geom_text(
      data = adae_sev %>%
        dplyr::filter(TRT01A != "Placebo")%>%
        dplyr::filter(AESER == "Y"),
      aes(
        y = incr+0.05,
        x = endd + 2,
        label = AEDECOD,
        group = AEDECOD,
        colour = color),
      size = 3,
      #position = position_dodge(width = 1),
      hjust=-0.1,
      fontface = "bold"
      #  check_overlap = TRUE,
    ) +
    scale_colour_manual(values = c("black" = "black", "red" = "red")) +
    facet_wrap(~ reorder(USUBJID, -TRTDUR),
               ncol = 1,
               strip.position= "right"
               ) +
    scale_x_continuous(limits = c(0,200)) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          #strip.background = element_blank(),
          strip.text.y = element_text(angle = 0)) +
    #strip.text.x = element_blank()) +
    xlab("Treatment duration (days)") +
    labs (
          title = title,
          subtitle = subtitle,
          caption = caption
          # title = "Reported adverse events (preferred terms) for patients with at least on-treatment severe adverse event",
          # subtitle = "Safety analysis set and received Xanomeline",
          # caption = "Circle = AE start day, diamond = AE end day (if reported).\n The grey box indicates individual patient Xanomeline exposure."
          )



  return(plot_04)

}
