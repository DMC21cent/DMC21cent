#' A laboratory level summary bar plot
#'
#' This functions creates a barplot for each treatment that shows
#' the percentage subjects below the lower limit of normal (LLN) and above
#' the upper limit of normal (ULN)
#'
#' @param adlb Laboratory dataset
#' @param kind The type of visualization. Options are `abnormal`, `box`, and `shift`. Default is `abnormal`.
#' @param param_filter Character. The filtration applies on `PARAM` variable.
#' @param selected_visit Vector of characters. The filtration on `AVISIT` variable.
#' @param title the main title of the plot.
#' @param shift_breaks_vec the break x & y axis for shift plot.
#' @param shift_alpha the opacity of the shift plot dots
#' @param shift_size the size of the shift plot dots
#'
#' @import ggplot2
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'
#'  load("data/adlbc.rda")
#'
#'  lab_summary(adlb = adlbc,
#'              kind = "abnormal",
#'              param_filter = "Cholesterol (mmol/L)",
#'              selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment")
#'             )
#'
#'  lab_summary(adlb = adlbc,
#'              kind = "box",
#'              param_filter = "Cholesterol (mmol/L)",
#'              selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment")
#'             )
#'
#' }
lab_summary <- function(adlb,
                        kind = "abnormal",
                        param_filter,
                        title = NULL,
                        shift_breaks_vec = NULL,
                        shift_alpha = 0.5,
                        shift_size = 3,
                        selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment")) {



  if(kind == "abnormal"){
  stats <- adlb %>%
    dplyr::group_by(PARAM, AVISIT, TRTA, LBNRIND) %>%
    dplyr::summarise(n = dplyr::n())


  big_n <- adlb %>%
    dplyr::group_by(PARAM, TRTA, AVISIT) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::ungroup()


  # adlb %>%
  #   dplyr::filter(PARAM %in% param_filter) %>%
  #   dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
  #   filter(AVISIT %in% selected_visit) %>%
  #   miss_var_summary()



  big_n_with_label <- big_n %>%
    dplyr::filter(PARAM %in% param_filter) %>%
    dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
    dplyr::group_by(PARAM, AVISIT) %>%
    mutate(AVISTTF = factor(AVISIT, levels = selected_visit)) %>%
    dplyr::summarise(avisit_with_label =  paste0(AVISTTF, "\n", paste0("(n_", substr(TRTA,1, 1) %>% toupper(), "=", N,")", collapse = "\n"))) %>%
    dplyr::ungroup()


   all_stats <-
    big_n %>%
    dplyr::left_join(stats) %>%
    dplyr::mutate(
      AVISIT = trimws(AVISIT, "l"),
      AVISTTF = factor(
        AVISIT,
        levels = selected_visit
      ),
      perc = 0 + n/N * 100,
      all_perc = ifelse(LBNRIND == "HIGH",
                        perc,-1 * perc))

  all_stats2 <-
    all_stats %>%
    tidyr::pivot_wider(names_from = LBNRIND,
                              values_from = c(perc, all_perc, n, N))

  DDD <- all_stats2 %>%
    dplyr::filter(PARAM %in% param_filter &
                  AVISIT %in% selected_visit) %>%
    dplyr::left_join(big_n_with_label %>% select(AVISIT, avisit_with_label)) %>%
    mutate(avisit_with_label = as.factor(avisit_with_label))

  a <- DDD %>%
    ggplot(aes(x = forcats::fct_relevel(avisit_with_label,
                                        stringr::str_subset(levels(DDD$avisit_with_label), pattern = "^End of Treatment"),
                                        after = Inf) ,
               y = perc_HIGH,
               fill = TRTA)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, width = 0.6) +
    theme_minimal(base_size = 12) +
    xlab("") +
    ylab("") +
    geom_hline(yintercept = 0)+
    scale_y_continuous(limits = c(0, 8)) +
    ggtitle("%subjects above the upper limit of normal (ULN)") +
    guides(fill = guide_legend(title = "")) +
    theme(
      axis.text.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), color = "black"),
      axis.text.y = element_text(color = "black"),
      #legend.position = "none",
      legend.position = c(0.8, 0.8),
      legend.direction="horizontal",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank())

  b <- DDD %>%
    ggplot(aes(x = forcats::fct_relevel(avisit_with_label,
                                        stringr::str_subset(levels(DDD$avisit_with_label), pattern = "^End of Treatment"),
                                        after = Inf),
               y = perc_LOW,
               label = round(perc_LOW,1),
               fill = TRTA)) +
    geom_bar(stat = "identity", position=position_dodge(), alpha = 0.8, width = 0.6) +
    #geom_text(hjust = 2.5, colour = "black", position=position_dodge()) +
    theme_minimal(base_size = 12) +
    xlab("") +
    ylab("") +
    #scale_y_continuous(limits = c(0, 8)) +
    scale_y_reverse(limits = c(8, 0)) +
    geom_hline(yintercept = 0)+
    labs(caption="%subjects below the lower limit of normal (LLN)") +
    theme(plot.caption = element_text(hjust=0, size=rel(1.2))) +
    #ggtitle("%subjects below the lower limit of normal (LLN)") +
    guides(fill = guide_legend(title = "")) +
    theme(
      legend.position = "none",
      axis.text.y = element_text(color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_blank()
      )


  ## merge plots - top and bottom
  plot_output <- a/b

  if(!is.null(title)){
    plot_output <- plot_output +
      patchwork::plot_annotation(title = title)
  } else {
    plot_output <- plot_output +
      patchwork::plot_annotation(title = glue::glue("{param_filter} by visit and treatment. Safety analysis set."))
  }

  } else if(kind == "box"){

    limits <- adlb %>%
      dplyr::filter(PARAM %in% param_filter) %>%
      dplyr::slice(1) %>%
      dplyr::select(A1LO, A1HI)


    big_n_with_label <- adlb %>%
      dplyr::group_by(PARAM, TRTA, AVISIT) %>%
      dplyr::summarise(N = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(PARAM %in% param_filter) %>%
      dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
      dplyr::group_by(PARAM, AVISIT) %>%
      dplyr::summarise(avisit_with_label = paste0(AVISIT, "\n", paste0("(n_", substr(TRTA,1, 1) %>% toupper(), "=", N,")", collapse = "\n"))) %>%
      dplyr::ungroup()


    DD <- adlb %>%
      dplyr::filter(PARAM %in% param_filter) %>%
      dplyr::mutate(
        AVISIT = trimws(AVISIT, "l"),
        AVISTTF = factor(
          AVISIT,
          levels = selected_visit
        ))%>%
      dplyr::filter(AVISIT %in% selected_visit) %>%
      dplyr::left_join(big_n_with_label %>% select(AVISIT, avisit_with_label)) %>%
      mutate(avisit_with_label = as.factor(avisit_with_label))


    plot_output <- DD %>%
      ggplot(aes(x = forcats::fct_relevel(avisit_with_label,
                                          stringr::str_subset(levels(DD$avisit_with_label), pattern = "^End of Treatment"),
                                          after = Inf),
                 y = AVAL,
                 fill = TRTA)) +
      geom_boxplot() +
      theme_minimal(base_size = 12) +
      geom_hline(yintercept = limits$A1LO, colour = "black")+
      geom_hline(yintercept = limits$A1HI)+
      annotate("text", x = 7, y = limits$A1HI + 0.5, size = 4, label = "Upper limit of Normal", color = "black", alpha = 0.5) +
      annotate("text", x = 7, y = limits$A1LO - 0.5, size = 4, label = "Lower limit of Normal", color = "black", alpha = 0.5) +
      guides(fill = guide_legend(title = "")) +
      xlab("") +
      ylab(glue::glue("{param_filter}")) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


    if(!is.null(title)){
      plot_output <- plot_output +
        ggtitle(title)
    } else {
      plot_output <- plot_output +
        ggtitle(glue::glue("{param_filter} by visit and treatment. Safety analysis set."))
    }



  } else if(kind == "shift"){


    adlbhy_proc <- adlb %>%
      dplyr::filter(SAFFL == "Y", !is.na(BASE), AVISITN > 0) %>%
      dplyr::filter(PARAM %in% param_filter)


    max_value <- max(
      max(adlbhy_proc %>% dplyr::pull(AVAL) %>% range()),
      max(adlbhy_proc %>% dplyr::pull(BASE) %>% range())
    )

    # y_diff <- adlbhy_proc %>% dplyr::pull(AVAL) %>% range() %>% diff()
    # x_diff <- adlbhy_proc %>% dplyr::pull(BASE) %>% range() %>% diff()
    # asp_ratio <- x_diff/y_diff


    low <- adlbhy_proc %>%
      dplyr::slice(1) %>%
      dplyr::pull(A1LO)

    high <- adlbhy_proc %>%
      dplyr::slice(1) %>%
      dplyr::pull(A1HI)


    plot_output <- adlbhy_proc %>%
      ggplot(aes(y = AVAL, x = BASE, group = TRTP, colour = TRTP)) +
      geom_point(alpha = shift_alpha, size = shift_size) +
      geom_vline(xintercept = as.numeric(low)) +
      geom_vline(xintercept = as.numeric(high)) +
      geom_hline(yintercept = as.numeric(low)) +
      geom_hline(yintercept = as.numeric(high)) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title=element_blank()
      ) +
      coord_fixed(ratio = 1) +
      xlab("Baseline") +
      ylab("Worst post-baseline")

    if(is.null(shift_breaks_vec)){

      labels = seq(0, max_value + 0.001, 25) %>% as.character()

      for(n in 1:length(labels)){
        if(n %% 2 == 0){
          labels[n] = ""
        }
      }

      plot_output <- plot_output +
        scale_x_continuous(limits = c(0, max_value + 5), breaks = seq(0, max_value + 0.001, 25), labels = labels) +
        scale_y_continuous(limits = c(0, max_value + 5), breaks = seq(0, max_value + 0.001, 25), labels = labels)

    } else {
      plot_output <- plot_output +
        scale_x_continuous(limits = c(0, max_value + 5), breaks = shift_breaks_vec) +
        scale_y_continuous(limits = c(0, max_value + 5), breaks = shift_breaks_vec)

    }



    if(!is.null(title)){
      plot_output <- plot_output +
        ggtitle(title)
    } else {
      plot_output <- plot_output +
        ggtitle(glue::glue("Shift plot of {param_filter} "))
    }


    } else {
    stop("The `kind` parameter should be 'abnormal', 'box', or 'shift'.")
  }




  return(plot_output)

}




