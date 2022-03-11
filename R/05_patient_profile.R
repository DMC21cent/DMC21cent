#' A Patient Profile function
#'
#' This functions creates a spaghetti plot for each lab parameter
#'
#' @param data dataset
#' @param kind The type of visualization. Options are `summary`, `lab`, `ae`, `cm`. The data domain should be matched with the kind of visualization.
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
#'

patient_profile <- function(data,
                            usubjid = "02-718-1371",
                            kind = 'summary',
                            ncol_lab = 1,
                            filter_safety_flag_lab = "Y",
                            transform_lab = 'log',
                            height_ae = 250,
                            order_servere_ae = c("MILD", "MODERATE", "SEVERE"),
                            color_ae = sample_colors[c('Blue Light', 'Apricot', 'Red')],
                            color_cm = sample_colors[c('Purple')],
                            str_wrap_ae_cm = 25,
                            params_lab = c("Alanine Aminotransferase (U/L)",
                                   "Aspartate Aminotransferase (U/L)",
                                   "Bilirubin (umol/L)"),
                            # cm_filters = c("ASPIRIN", "CALCIUM"),
                            # ae_filters = c("FATIGUE", "DIZZINESS", "ERYTHEMA"),
                            selected_visit = c("Baseline", "Week 2", "Week 4", "Week 6", "Week 8", "End of Treatment")) {

if(kind == 'summary'){

  data %>%
    filter(USUBJID == usubjid) %>%
    select(USUBJID, SEX, RACE, AGE, ARM, SITEID, TRTSDT, TRTEDT) %>%
    rename("First Exposure to TRT" = TRTSDT,
           "Last Exposure to TRT" = TRTEDT) %>%
    kableExtra::kable() %>%
    kableExtra::kable_styling(font_size = 14,
                              full_width = F,
                              #latex_options = "HOLD_position",
                              position = "center")



} else if(kind == 'lab') {


  # data_lab <- data %>%
  #   filter(USUBJID == usubjid) %>%
  #   filter(PARAM %in% params_lab) %>%
  #   # dplyr::filter(SAFFL == "Y", !is.na(BASE), AVISITN > 0) %>%
  #   dplyr::filter(SAFFL == filter_safety_flag_lab) %>%
  #   dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
  #   dplyr::filter(AVISIT %in% selected_visit) %>%
  #   mutate(AVISTTF = factor(AVISIT, levels = selected_visit))
  #
  # lab_plots <- data_lab %>%
  #   ggplot() +
  #   geom_line(aes(x = ADT, y = AVAL, group = USUBJID)) +
  #   facet_wrap(PARAM ~ ., scales = "free_y", ncol = ncol_lab) +
  #   geom_hline(aes(yintercept = A1LO), linetype = "dashed", size = .3) +
  #   geom_hline(aes(yintercept = A1HI), linetype = "dashed", size = .3) +
  #   ylab("") +
  #   xlab("Visit") +
  #   geom_vline(xintercept = data_lab %>% pull("TRTSDT") %>% unique(), linetype="dotted") +
  #   geom_vline(xintercept = data_lab %>% pull("TRTEDT") %>% unique(), linetype="dotted") +
  #   scale_x_date(
  #     breaks = data_lab %>% pull(ADT) %>% unique(),
  #     labels = data_lab %>% pull(AVISTTF) %>% unique())


  lab_plots <- data %>%
    filter(USUBJID == usubjid) %>%
    filter(PARAM %in% params_lab) %>%
    # dplyr::filter(SAFFL == "Y", !is.na(BASE), AVISITN > 0) %>%
    dplyr::filter(SAFFL == filter_safety_flag_lab) %>%
    dplyr::mutate(AVISIT = trimws(AVISIT, "l")) %>%
    dplyr::filter(AVISIT %in% selected_visit) %>%
    mutate(AVISTTF = factor(AVISIT, levels = selected_visit)) %>%
    ggplot() +
    geom_line(aes(x = AVISTTF, y = AVAL, group = USUBJID)) +
    facet_wrap(PARAM ~ ., scales = "free_y", ncol = ncol_lab) +
    geom_hline(aes(yintercept = A1LO), linetype = "dashed", size = .3) +
    geom_hline(aes(yintercept = A1HI), linetype = "dashed", size = .3) +
    ylab("") +
    xlab("Visit") +
    theme_minimal()


  if(transform_lab == 'log'){
    lab_plots <- lab_plots +
      scale_y_log10()
  }

  return(lab_plots)

} else if(kind == 'ae') {


  df <- data %>%
    filter(USUBJID == usubjid) %>%
    select(USUBJID, AEDECOD, ASTDT, AENDT, TRTSDT, TRTEDT,  AESEV, AESER)


  df_distinct_severe <- df %>%
    dplyr::select(AEDECOD, ASTDT, AENDT, AESEV) %>%
    dplyr::group_by(AEDECOD, ASTDT, AENDT) %>%
    tidyr::nest() %>%
    mutate(AESEV_mod = purrr::map_chr(data, function(x){

      index_most_severe <- (order_servere_ae %in% (x %>% unlist())) %>%
        which(. == TRUE) %>%
        tail(1)

      return(order_servere_ae[index_most_severe])

    })) %>%
    select(-data)

df <- df %>%
  left_join(df_distinct_severe) %>%
  select(-AESEV) %>%
  distinct() %>%
  rename("AESEV" = "AESEV_mod")



  col = color_ae
  colorNames <- adae %>% pull(AESEV) %>% unique()
  col <- setNames(col, colorNames)



  df %>%
    #group_by(AEDECOD) %>%
    mutate(AEDECOD_wrapped =  AEDECOD %>% stringr::str_wrap(width = str_wrap_ae_cm) %>%
             stringr::str_replace_all(pattern = "\\n", "  \n")) %>%
    plot_ly(
      height = height_ae
      # hoverinfo = "text",
      # text = ~ note
    ) %>%
  #######################################
  # add line segment start and end date #
  #######################################
  add_segments(x = ~.data[[ "ASTDT" ]], xend = ~.data[[ "AENDT" ]],
               y = ~.data[[ "AEDECOD_wrapped" ]], yend = ~.data[[ "AEDECOD_wrapped" ]],
               color = ~.data[[ "AESEV" ]],
               colors = col, #size = I( 5 )
               opacity = 1,
               line= list( width=5 ),
               showlegend = F
  ) %>%
  ###################################
  # add marker at the AE start date #
  ###################################
  add_markers (x = ~.data[[ "ASTDT" ]],
               y = ~.data[[ "AEDECOD_wrapped" ]],
               color = ~.data[[ "AESEV" ]],
               colors = col,
               marker = list(size = 8, opacity = 1, line = list( width=1 )),
               #hoverinfo = "text"
  ) %>%
  ###################################
  # add marker at missing end date
  ###################################
  add_markers(data = df %>%
                mutate(AEDECOD_wrapped =  AEDECOD %>% stringr::str_wrap(width = str_wrap_ae_cm) %>%
                         stringr::str_replace_all(pattern = "\\n", "  \n")) %>%
                filter(is.na(.data[[ "AENDT" ]]) & !is.na(.data[[ "ASTDT" ]])) %>%
                mutate(Message = "Missing end date"),
              x = ~ .data[[ "ASTDT" ]] + 1,
              y = ~.data[[ "AEDECOD_wrapped" ]],
              color = ~.data [[ "Message" ]],
              marker = list(size = 6, opacity = 0.75, color = " #696969",
                            line = list( width=.8, color="#000000"), symbol = 'star-dot'),
              text = ~.data [[ "Message" ]],
              hoverinfo = "text"
  ) %>%
  ###################################
  # add marker at missing start and end date
  ###################################
  add_markers(data = df %>%
                mutate(AEDECOD_wrapped =  AEDECOD %>% stringr::str_wrap(width = str_wrap_ae_cm) %>%
                         stringr::str_replace_all(pattern = "\\n", "  \n")) %>%
                filter(!is.na(.data[[ "AENDT" ]]) & is.na(.data[[ "ASTDT" ]])) %>%
                mutate(Message = "Missing start date"),
              x = ~ .data[[ "AENDT" ]] - 1,
              y = ~.data[[ "AEDECOD_wrapped" ]],
              color = ~.data [[ "Message" ]],
              marker = list(size = 6, opacity = 0.75, color = " #696969",
                            line = list( width=.8, color="#000000"), symbol = 'cross'),
              text = ~.data [[ "Message" ]],
              hoverinfo = "text"
  ) %>%
  ###################################
  # add marker at missing start and end date
  ###################################
  add_markers(data = df %>%
                mutate(AEDECOD_wrapped =  AEDECOD %>% stringr::str_wrap(width = str_wrap_ae_cm) %>%
                         stringr::str_replace_all(pattern = "\\n", "  \n")) %>%
                filter(is.na(.data[[ "AENDT" ]]) & is.na(.data[[ "ASTDT" ]])) %>%
                mutate(Message = "Missing both start and end date"),
              x = ~ .data[[ "TRTSDT" ]] + 1,
              y = ~.data[[ "AEDECOD_wrapped" ]],
              color = ~.data [[ "Message" ]],
              marker = list(size = 6, opacity = 0.75, color = " #696969",
                            line = list( width=.8, color="#000000"), symbol = 'square-x'),
              text = ~.data [[ "Message" ]],
              hoverinfo = "text"
  ) %>%
  ##################################
  # add special marker for each SAE #
  ###################################
  add_markers (data = df %>%
                 mutate(AEDECOD_wrapped =  AEDECOD %>% stringr::str_wrap(width = str_wrap_ae_cm) %>%
                          stringr::str_replace_all(pattern = "\\n", "  \n")) %>%
                 filter( .data[[ "AESER" ]]  =='Y') %>%
                 mutate(ser_label = "SERIOUS"),
               x = ~.data[[ "ASTDT" ]], y = ~.data[[ "AEDECOD_wrapped" ]],
               color = ~.data [[ "ser_label" ]],
               marker = list(size = 18, opacity = 0.6, line = list( width=3, color=I('black'))),
               #hoverinfo = "text"
  ) %>%
    ###############################################################
  # add subtitle and vertical lines for study start / end dates #
  ###############################################################
  layout( title = "Adverse Events",
          yaxis  = list( title = '', zeroline = FALSE, ticksuffix = "   " ),
          xaxis = list(title = '', tickformat="%B %d"),
          shapes = list(vline(df %>% pull("TRTSDT") %>% unique()),
                        vline(df %>% pull("TRTEDT") %>% unique()),
                        hline(-1)),
          legend = list(orientation = 'h'))






} else if(kind == 'cm') {


  df <- data %>%
    filter(USUBJID == usubjid) %>%
    select(USUBJID, CMTRT, ASTDT, AENDT, TRTSDT, TRTEDT, CMSTDTC, CMENDTC, ONTRTFL)


  col = color_cm


  df %>%
    filter(ONTRTFL == "Y") %>%
    #group_by(CMTRT) %>%
    mutate(CMTRT_wrapped =  CMTRT %>% stringr::str_wrap(width = str_wrap_ae_cm)) %>%
    plot_ly(
      height = height_ae
      # hoverinfo = "text",
      # text = ~ note
    ) %>%
    #######################################
  # add line segment start and end date #
  #######################################
  add_segments(data =   df %>%
                 mutate(CMTRT_wrapped =  CMTRT %>% stringr::str_wrap(width = str_wrap_ae_cm)) %>%
                 mutate(cm_col = "CM"),
               x = ~.data[[ "ASTDT" ]], xend = ~.data[[ "AENDT" ]],
               y = ~.data[[ "CMTRT_wrapped" ]], yend = ~.data[[ "CMTRT_wrapped" ]],
               color = col,
               colors = "#911EB4", #size = I( 5 )
               opacity = 1,
               line= list( width=5 ),
               text = ~.data[[ 'AENDT' ]],
               hoverinfo = "text"
  ) %>%
  ###################################
  # add marker at the AE start date #
  ###################################
  add_markers (x = ~.data[[ "ASTDT" ]], y = ~.data[[ "CMTRT_wrapped" ]],
               color = col,
               colors = "#911EB4",
               marker = list(size = 8, opacity = 1, line = list( width=1)),
               text = ~.data[[ 'ASTDT' ]],
               hoverinfo = "text"
  ) %>%
  layout( title = "Concomitant Medications",
          margin = list(l = 205, r = 2),
          yaxis  = list(title = '', zeroline = FALSE, ticksuffix = "   "),
          xaxis = list(title = '', range=c(df %>% pull("TRTSDT") %>% unique(),
                                           df %>% pull("TRTEDT") %>% unique()), tickformat="%B %d"),
          showlegend = FALSE,
          shapes = list(vline(df %>% pull("TRTSDT") %>% unique()),
                        vline(df %>% pull("TRTEDT") %>% unique()),
                        hline(-1)))


} else {
  return(invisble(NULL))
}


}




#-------------------------------
# Novartis colors
#--------------------------------
sample_colors <- c(
  `Blue Dark`        = "#023761",
  `Novartis Blue`    = "#0460A9",
  `Blue Light`       = "#5291DD",
  `Carmine Dark`     = "#5E1619",
  `Carmine`          = "#8D1F1B",
  `Carmine Light`    = "#D13A32",
  `Sienna Dark`      = "#B34534",
  `Sienna`           = "#E74A21",
  `Sienna Light`     = "#F58144",
  `Apricot Dark`     = "#BF7D1A",
  `Apricot`          = "#EC9A1E",
  `Apricot Light`    = "#FCB13B",
  `Gray`             = "#9D9D9C",
  `Gray Medium`      = "#C6C6C6",
  `Gray Light`       = "#EDEDED",
  `Green`            = "#3CB44B",  ## add external colors
  `Green Light`      = "#32CD32",
  `Red`              = "#CC0022",
  `Purple`           = "#911EB4",
  `Cyan`             = "#46F0F0",
  `Magenta`          = "#F032E6",
  `Lime`             = "#D2F53C",
  `Pink`             = "#E68FAC",
  `Teal`             = "#008080",
  `Brown`            = "#AA6E28",
  `Mint`             = "#AAFFC3",
  `Black`            = "#000000",
  `Olive`            = "#808000",
  `Salmon`           = "#F8766D")



#-------------------------------
# add reference line
#--------------------------------
vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash = 'dot')
  )
}

hline <- function(y = 0, color = "black") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, width =2)
  )
}

# ggplot(mpg, aes(displ, hwy)) +
#   geom_point() +
#   scale_x_continuous(limits = c(1, 7), expand = c(-0.01, 0))
# # right most position will be 7 + (7-1) * 0.5 = 10
#
#
#



