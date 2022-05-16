#' Comparing Adverse Events (preferred terms) of interest by treatment
#'
#' This function compares different adverse events using multiple visualization
#' methods: Forest plot, Rate plot, Swimmer plot, Volcano Plot for P-values, and Volcano Plots for Adjusted P-values.
#'
#' @note for the `forest`, `volcano_pval`, and `volcano_adjpval`, this function
#' calculates hazard ratio based on the following `Cox` regression model:
#' \code{coxph(Surv(AVAL, CNSR) ~ TRT01A)}
#'
#' @param adae Adverse Event dataset
#' @param adsl Subject Level dataset
#' @param kind The type of visualization. Options are `forest`, `rate`,
#' `volcano_pval`, and `volcano_adjpval`. Default is `forest`.
#' @param pval_cutoff A cut-off value for filtering the hazard output results
#'   on `p.value < pval_cutoff`. No effects on the `rate` and `swimmer` plots.
#'   Default is `1`.
#' @param adjpval_cutoff A cut-off value for showing the text on the `volcano_pval` and `volcano_adjpval outputs`.
#'   It will apply a filtration on `adjpvalue < adjpval_cutoff`.
#'   No effects on the `rate` and `swimmer` plots. Default is `0.9`.
#' @param title A string shows the main title plot.
#' @param subtitle A string shows the subtitle plot.
#' @param caption A string shows the caption of plot.
#' @param reord_type_rateplot
#'
#' @import ggplot2
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  load("data/adsl.rda")
#'  load("data/adae.rda")
#'
#'  ae_summary(adae = adae,
#'             adsl = adsl,
#'             kind = "forest",
#'             pval_cutoff = 0.6
#'            )
#'
#'  ae_summary(adae = adae,
#'             adsl = adsl,
#'             kind = "rate"
#'            )
#'
#'  ae_summary(adae = adae,
#'             adsl = adsl,
#'             kind = "swimmer"
#'            )
#'
#'  ae_summary(adae = adae,
#'             adsl = adsl,
#'             kind = "volcano_pval",
#'             pval_cutoff = 1,
#'             adjpval_cutoff = 0.9
#'            )
#'
#'
#'  ae_summary(adae = adae,
#'             adsl = adsl,
#'             kind = "volcano_adjpval",
#'             pval_cutoff = 1,
#'             adjpval_cutoff = 0.9
#'             )
#' }
ae_summary <- function(adae,
                       adsl,
                       kind = "forest",
                       pval_cutoff = 1,
                       adjpval_cutoff = 0.9,
                       reord_type_rateplot = "diff",
                       title = "",
                       subtitle = "",
                       caption = "") {

 if(kind == "rate"){
   ae_rate_plot(adae, adsl, reord_type_rateplot, title, subtitle, caption)
 } else if(kind == "swimmer"){
   ae_swimmer_plot(adae, adsl, title, subtitle, caption)
 } else {
  pts_sub <-
    adae %>%
    dplyr::group_by(USUBJID, AEDECOD) %>%
    dplyr::tally()

  ## pivot wider to identify patients without event and set to zero
  pts_sub2 <-
    pts_sub %>%
    tidyr::pivot_wider(names_from = AEDECOD , values_from = n, values_fill = 0)

  ## transform to long again for processing
  pts_sub3 <-
    pts_sub2 %>%
    tidyr::pivot_longer(-USUBJID, names_to = "AEDECOD", values_to = "n")

  ## calculate the censoring information per event.
  ## Take first event per patient
  ## Note - it looks like original tables calculated crude incidence
  cnsr <- adae %>%
    dplyr::filter(AOCCPFL == "Y") %>%
    dplyr::select(USUBJID, AEDECOD, TRTSDT, ASTDT) %>%
    dplyr::group_by(USUBJID, AEDECOD) %>%
    dplyr::mutate(d = as.double(difftime(lubridate::ymd(ASTDT),
                                  lubridate::ymd(TRTSDT),
                                  units = "days")) + 1)

  pts_sub4 <-
    pts_sub3 %>%
    dplyr::left_join(cnsr)

  patients <- adsl %>%
    dplyr::select(USUBJID, TRTDUR, TRT01A)

  pts_sub5 <-
    pts_sub4 %>%
    dplyr::left_join(patients) %>%
    dplyr::mutate(AVAL = dplyr::if_else(is.na(d), TRTDUR, d),
           CNSR = dplyr::if_else(n > 0, 1, 0)) %>%
    dplyr::select(-c(TRTDUR, d, TRTSDT, ASTDT))



  ########################################
  ## calculate hazard ratios per PT
  ########################################
  hazard <-
    pts_sub5 %>%
    tidyr::nest(data = -AEDECOD) %>%
    dplyr::mutate(
      outputs = purrr::map(data, ~ survival::coxph(survival::Surv(AVAL, CNSR) ~ TRT01A, data = .)),
      tidyoutputs = purrr::map(outputs, ~ try(broom::tidy(x = ., conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE)))) %>%
    tidyr::unnest(tidyoutputs) %>%
    dplyr::select(-c(data, outputs))  %>%
    dplyr::filter(term !="(Intercept)") %>%
    dplyr::mutate(adjpvalue = p.adjust(p.value, method="fdr"),) %>%  ## adjusted pvalues
    dplyr::mutate(AEDECOD = stringr::str_to_title(AEDECOD)) ## PT to title case




  ########################################
  ## Plot volcano plot
  ########################################


  ### volcano plot
  # hazard %>% glimpse()
  #
  # a <- hazard %>%
  #   filter(p.value < 0.6)

  if(kind == "forest"){
  plot_output <- hazard %>%
    dplyr::filter(p.value < pval_cutoff) %>%
    ggplot(aes(
      y = reorder(AEDECOD, estimate),
      x = estimate,
      xmin = conf.low,
      xmax = conf.high
    )) +
    geom_vline(
      xintercept = 1,
      color = 'grey',
      alpha = 0.6,
      size = 1
    ) +
    geom_pointrange(alpha = 0.5,
                    size = 0.75,
                    colour = "black") +
    scale_x_continuous(breaks = c(0.25, 0.5, 1, 2, 4, 8),
                       trans = scales::log_trans()) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(size = 0.3),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text.y = element_text(angle = 0),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    xlab("Hazard ratio") +
    ylab("p-value") +
    labs (title = title,
          subtitle = ifelse(pval_cutoff == 1,
                            subtitle,
                            glue::glue("{subtitle} for P.value < {pval_cutoff}")),
          caption = caption)

  } else if(kind == "volcano_pval") {

    plot_output <- hazard %>%
      dplyr::filter(p.value < pval_cutoff) %>%
      # filter(adjpvalue < 0.99) %>%
      ggplot(aes(x = estimate, y = p.value)) +
      geom_vline(xintercept = 1, color = 'grey', alpha = 0.6, size = 1) +
      geom_hline(yintercept = 0.05, color = 'grey30', linetype = "dashed") +
      geom_hline(yintercept = 0.01, color = 'grey30', linetype = "dotted") +
      geom_text(
        data = hazard %>%
          dplyr::filter(adjpvalue < adjpval_cutoff),
        aes(x = estimate, y = p.value, label  = AEDECOD),
        size = 3.5,
        position = position_dodge(width = 1),
        hjust = 0.75,
        vjust = -0.25) +
      geom_point(size = 2, colour = "red" , alpha = 0.5 ) +
      scale_x_continuous(limits = c(0.1, 10),
                         breaks = c(0.2, 0.5, 1, 2,4, 6, 10),  #
                         trans = scales::log_trans()) +
      scale_y_continuous(trans = ggforce::trans_reverser('log10'),
                         breaks = c(0.1, 0.05, 0.01)) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none",
            #        panel.grid.major.y = element_blank(),
            #        panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            #        axis.text.y = element_blank(),
            #        axis.title.y = element_blank(),
            #strip.background = element_blank(),
            strip.text.y = element_text(angle = 0)) +
      #strip.text.x = element_blank()) +
      xlab("Hazard ratio") +
      ylab("p-value") +
      annotate("text", x = 0.2, y = 0.9, size = 4, label = " <--- Benefit.", color = "black", alpha = 0.5) +
      annotate("text", x = 6, y = 0.9, size = 4, label = "Harm --->", color = "black", alpha = 0.5) +
      labs (title = title,
            subtitle = ifelse(pval_cutoff == 1,
                              subtitle,
                              glue::glue("{subtitle} for P.value < {pval_cutoff}")),
            caption = caption)


  } else if(kind == "volcano_adjpval") {

    plot_output <- hazard %>%
      dplyr::filter(p.value < pval_cutoff) %>%
      ggplot(aes(x = estimate, y = adjpvalue)) +
      geom_vline(xintercept = 1, color = 'grey', alpha = 0.6, size = 1) +
      geom_hline(yintercept = 0.05, color = 'grey30', linetype = "dashed") +
      geom_hline(yintercept = 0.01, color = 'grey30', linetype = "dotted") +
      geom_text(
        data = hazard %>%
          dplyr::filter(adjpvalue < adjpval_cutoff),
        aes(x = estimate, y = adjpvalue, label  = AEDECOD),
        size = 3.5,
        position = position_dodge(width = 1),
        check_overlap = TRUE,
        hjust = 0.75,
        vjust = -0.25) +
      geom_point(size = 2, colour = "red" , alpha = 0.5 ) +
      scale_x_continuous(limits = c(0.1, 10),
                         breaks = c(0.2, 0.5, 1, 2,4, 6, 10),  #
                         trans = scales::log_trans()) +
      scale_y_continuous(trans = ggforce::trans_reverser('log10'),
                         breaks = c(0.1, 0.05, 0.01)) +
      theme_minimal(base_size = 18) +
      theme(legend.position = "none",
            panel.grid.minor.x = element_blank(),
            strip.text.y = element_text(angle = 0)) +
      xlab("Hazard ratio") +
      ylab("FDR adjusted p-value") +
      annotate("text", x = 0.2, y = 0.009, size = 4, label = " <--- Benefit.", color = "black", alpha = 0.5) +
      annotate("text", x = 6, y = 0.009, size = 4, label = "Harm --->", color = "black", alpha = 0.5) +
      labs (title = title,
            subtitle = ifelse(pval_cutoff == 1,
                              subtitle,
                              glue::glue("{subtitle} for P.value < {pval_cutoff}")),
            caption = caption)


  }

  return(plot_output)
}

}



