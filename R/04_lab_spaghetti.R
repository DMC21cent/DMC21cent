#' A laboratory spaghetti plot
#'
#' This functions creates a spaghetti plot for each lab parameter
#'
#' @param adlb Laboratory dataset
#' @param param_filter Character. The filtration applies on `PARAM` variable.
#' @param selected_visit Vector of characters. The filtration on `AVISIT` variable.
#' @param x_axis_wrap wrap long text in the x axis.
#' @param axis_tick_size the size of the axis tick.
#'
#' @import ggplot2
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#' }
lab_spaghetti <- function(adlb,
                        param_filter,
                        axis_tick_size = 8,
                        x_axis_wrap = 6,
                        selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment")) {


  adlbhy_proc <- adlb %>%
    # dplyr::filter(SAFFL == "Y", !is.na(BASE), AVISITN > 0) %>%
    dplyr::filter(SAFFL == "Y") %>%
    dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
    dplyr::filter(AVISIT %in% selected_visit) %>%
    dplyr::filter(PARAM %in% param_filter)


  low <- adlbhy_proc %>%
    dplyr::pull(A1LO) %>%
      unique()

  high <- adlbhy_proc %>%
    dplyr::pull(A1HI) %>%
    unique()


  if(length(low) == 1 & length(high) == 1){



    Data <- adlb %>%
      select(USUBJID, PARAM, PARAMCD, AVISIT, TRTA, AVAL, A1HI) %>%
      dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
      dplyr::filter(PARAM %in% param_filter) %>%
      dplyr::filter(AVISIT %in% selected_visit) %>%
      mutate(AVISTTF = factor(AVISIT, levels = selected_visit)) %>%
      select(USUBJID, TRTA, AVISTTF, AVAL, A1HI) %>%
      filter(!is.na(AVAL))


    Data_abnormal <- Data %>%
      mutate(abnormal_ind = ifelse(AVAL > 3*A1HI, T, F)) %>%
      group_by(USUBJID) %>%
      summarise(abnormal_subj = ifelse(sum(abnormal_ind) == 0, F, T))


    Joined_Data <- Data %>%
      left_join(Data_abnormal, by = "USUBJID") %>%
      mutate(color = ifelse(!abnormal_subj, "#D3D3D3", USUBJID))


    manual_colors <- Joined_Data %>%
      select(color) %>%
      distinct() %>%
      mutate(color2 = scales::hue_pal()(Joined_Data %>%
                                       select(color) %>%
                                       distinct() %>% nrow())) %>%
      mutate(color2 = ifelse(color == "#D3D3D3", "#D3D3D3", color2)) %>%
      pull(color2)

    Joined_Data %>%
      ggplot2::ggplot() +
      #ggplot2::geom_line(aes(x = AVISTTF, y = AVAL, color = color, group = USUBJID), show.legend=FALSE) +
      ggplot2::geom_line(aes(x = AVISTTF, y = AVAL, color = color, group = USUBJID),
                         data = Joined_Data %>% filter(color == "#D3D3D3"),
                         alpha = 8/10,
                         show.legend=FALSE) +
      ggplot2::geom_line(aes(x = AVISTTF, y = AVAL, color = color, group = USUBJID),
                         data = Joined_Data %>% filter(color != "#D3D3D3"),
                         show.legend=FALSE) +
      ggrepel::geom_label_repel(aes(x = AVISTTF, y = AVAL, label = ifelse(abnormal_subj, USUBJID, ""),
                                    color = color),
                                data = Joined_Data %>% filter(AVISTTF == tail(selected_visit, 1)),
                                size = 2.5, show.legend = FALSE) +
      facet_wrap(TRTA ~ .) +
      scale_y_log10() +
      theme(axis.text.x = element_text(size = axis_tick_size)) +
      theme(axis.text.y = element_text(size = axis_tick_size)) +
      scale_color_manual(values = manual_colors) +
      theme_minimal() +
      xlab("Visit") +
      ylab("") +
      ggtitle(param_filter) +
      geom_hline(aes(yintercept = A1HI), linetype = "dashed", size = .3) +
      geom_text(aes(tail(selected_visit, 1), A1HI, label = "ULN", vjust = - 1), size = 2.5) +
      geom_text(aes(head(selected_visit, 1), A1HI, label = A1HI, hjust = 1, vjust = - 1), color = "#63666A", size = 2.5) +

      geom_hline(aes(yintercept = 2*A1HI), linetype = "dashed", size = .3) +
      geom_text(aes(tail(selected_visit, 1), 2*A1HI, label = "2×ULN", vjust = - 1), size = 2.5) +
      geom_text(aes(head(selected_visit, 1), 2*A1HI, label = 2*A1HI, hjust = 1, vjust = - 1), color = "#63666A", size = 2.5) +

      geom_hline(aes(yintercept = 3*A1HI), linetype = "dashed", size = .3) +
      geom_text(aes(tail(selected_visit, 1), 3*A1HI, label = "3×ULN", vjust = - 1), size = 2.5) +
      geom_text(aes(head(selected_visit, 1), 3*A1HI, label = 3*A1HI, hjust = 1, vjust = - 1), color = "#63666A", size = 2.5) +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_wrap))




  #
  #
  # max <- adlb %>%
  #   select(USUBJID, PARAM, PARAMCD, AVISIT, TRTA, AVAL) %>%
  #   dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
  #   dplyr::filter(PARAM %in% param_filter) %>%
  #   dplyr::filter(AVISIT %in% selected_visit) %>%
  #   mutate(AVISTTF = factor(AVISIT, levels = selected_visit)) %>%
  #   select(USUBJID, TRTA, AVISTTF, AVAL) %>%
  #   pull(AVAL) %>% max()

  # high %>% as.numeric()
  # max %>% as.numeric()
  # (max %>% as.numeric())/(high %>% as.numeric())

  # adlb %>%
  #   select(USUBJID, PARAM, PARAMCD, AVISIT, TRTA, AVAL, A1HI) %>%
  #   dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
  #   dplyr::filter(PARAM %in% param_filter) %>%
  #   dplyr::filter(AVISIT %in% selected_visit) %>%
  #   mutate(AVISTTF = factor(AVISIT, levels = selected_visit)) %>%
  #   filter(!is.na(AVAL)) %>%
  #   select(USUBJID, TRTA, AVISTTF, AVAL, A1HI) %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_line(aes(x = AVISTTF, y = AVAL, color = USUBJID, group = USUBJID)) +
  #   facet_wrap(TRTA ~ .) +
  #   gghighlight::gghighlight(max(AVAL) > 3*high,
  #                            calculate_per_facet = TRUE,
  #                            label_params = list(size = 3)) +
  #   ggtitle(param_filter) +
  #   #labs(caption = "Highlighted the subjects with values bigger than 3*High-range") +
  #   theme_minimal() +
  #   xlab("Visit") +
  #   ylab("") +
  #   geom_hline(aes(yintercept = A1HI), linetype = "dashed", size = .3) +
  #   geom_text(aes(tail(selected_visit, 1), A1HI, label = "ULN", vjust = - 1), size = 2.4) +
  #   geom_text(aes(head(selected_visit, 1), A1HI, label = A1HI, hjust = 1, vjust = - .3), color = "#63666A", size = 2.2) +
  #
  #   geom_hline(aes(yintercept = 2*A1HI), linetype = "dashed", size = .3) +
  #   geom_text(aes(tail(selected_visit, 1), 2*A1HI, label = "2×ULN", vjust = - 1), size = 2.4) +
  #   geom_text(aes(head(selected_visit, 1), 2*A1HI, label = 2*A1HI, hjust = 1, vjust = - .3), color = "#63666A", size = 2.2) +
  #
  #   geom_hline(aes(yintercept = 3*A1HI), linetype = "dashed", size = .3) +
  #   geom_text(aes(tail(selected_visit, 1), 3*A1HI, label = "3×ULN", vjust = - 1), size = 2.4) +
  #     geom_text(aes(head(selected_visit, 1), 3*A1HI, label = 3*A1HI, hjust = 1, vjust = - .3), color = "#63666A", size = 2.2) +
  #   scale_y_log10() +
  #   theme(axis.text.x = element_text(size = 6))

  } else {


    Data <- adlb %>%
      select(USUBJID, PARAM, PARAMCD, AVISIT, TRTA, AVAL, A1HI) %>%
      dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
      dplyr::filter(PARAM %in% param_filter) %>%
      dplyr::filter(AVISIT %in% selected_visit) %>%
      mutate(AVISTTF = factor(AVISIT, levels = selected_visit)) %>%
      select(USUBJID, TRTA, AVISTTF, AVAL, A1HI) %>%
      filter(!is.na(AVAL))


    Data_abnormal <- Data %>%
      mutate(abnormal_ind = ifelse(AVAL > 3*A1HI, T, F)) %>%
      group_by(USUBJID) %>%
      summarise(abnormal_subj = ifelse(sum(abnormal_ind) == 0, F, T))


    Joined_Data <- Data %>%
      left_join(Data_abnormal, by = "USUBJID") %>%
      mutate(color = ifelse(!abnormal_subj, "#D3D3D3", USUBJID))


    manual_colors <- Joined_Data %>%
      select(color) %>%
      distinct() %>%
      mutate(color2 = scales::hue_pal()(Joined_Data %>%
                                       select(color) %>%
                                       distinct() %>% nrow())) %>%
      mutate(color2 = ifelse(color == "#D3D3D3", "#D3D3D3", color2)) %>%
      pull(color2)

    Joined_Data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = AVISTTF, y = AVAL, color = color, group = USUBJID),
                         data = Joined_Data %>% filter(color == "#D3D3D3"),
                         alpha = 8/10,
                         show.legend=FALSE) +
      ggplot2::geom_line(aes(x = AVISTTF, y = AVAL, color = color, group = USUBJID),
                         data = Joined_Data %>% filter(color != "#D3D3D3"),
                         show.legend=FALSE) +
      ggrepel::geom_label_repel(aes(x = AVISTTF, y = AVAL, label = ifelse(abnormal_subj, USUBJID, ""),
                                   color = color),
                               data = Joined_Data %>% filter(AVISTTF == tail(selected_visit, 1)),
                               size = 2.5, show.legend = FALSE) +
      facet_wrap(TRTA ~ .) +
      ggtitle(param_filter) +
      scale_y_log10() +
      theme(axis.text.x = element_text(size = axis_tick_size)) +
      theme(axis.text.y = element_text(size = axis_tick_size)) +
      scale_color_manual(values = manual_colors) +
      theme_minimal() +
      xlab("Visit") +
      ylab("") +
      scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = x_axis_wrap))


  }


}

# param_filter ="Alanine Aminotransferase (U/L)"
# param_filter ="Alkaline Phosphatase (U/L)"
# param_filter ="Aspartate Aminotransferase (U/L)"
# param_filter ="Bilirubin (umol/L)"
# param_filter ="Creatine Kinase (U/L)"
# param_filter ="Gamma Glutamyl Transferase (U/L)"
#
# selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment")
#
#
# lab_spaghetti(adlb = adlb,
#               param_filter = param_filter,
#               axis_tick_size = 8,
#               selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment"))

# adlb %>%
#   dplyr::filter(SAFFL == "Y", !is.na(BASE)) %>%
#   dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
#   dplyr::filter(AVISIT %in% selected_visit)  %>%
#   dplyr::select(PARAM, A1LO) %>%
#   dplyr::group_by(PARAM) %>%
#   dplyr::summarise(n = dplyr::n_distinct(A1LO)) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(n) %>%
#   filter(n == 1)
#
