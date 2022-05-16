#' A subject level graphical summary
#'
#' Some systematic visualizations to display the characteristics of the study population
#'
#' @param data dataset with variables to plot. It generally is a subject level data (`adsl`).
#' @param vars the vector of baseline variables of interest to display.
#' @param kinds the type of graphical visualization corresponding to each element of `vars`.
#'   It may be a length one string means that graph will be used for all the varialbes of `vars`.
#'   Otherwise, `kinds` should have the same length of `vars`. In this case, each element of `kinds`, will
#'   be applied to its corresponding element of `vars`. `kinds` may take three options for each element:
#'   \itemize{
#'     \item{"box"}{special box plot, combining with rug plot, for `continuous` variables}
#'     \item{"bar_binom"}{bar plot for categorical variables. It shows only one of the levels.
#'        By default, it selects the high frequency level.
#'        For selecting the other level, you can follow the format
#'        of `list("var" = "level")` in the `vars`. Check the examples to see how it works}
#'     \item{"bar_stack"}{The stack bar chart for binomial or multinomial variables.}
#'    }
#' @param show_plots logical If `TRUE`, it combined all the plots and shows them using \code{\link[patchwork]{wrap_plots}}.
#'   For controlling the number of columns, check `ncol`
#' @param ncol numeric. If `show_plots` is `TRUE`, `ncol` controls the number of column. Default is 1.
#' @param titles the vector of main titles for each graph. By default all is `NA` which means no title.
#' @param trt the column name for planned treatment. Default is `TRT01P`.
#' @param relevel_list A list of vectors that applies a relevel on the `var` levels. It affect the order
#'   of stacked bar plots. It only affects `bar_stack` kinds.
#' @param breaks_vec A list of vectors that breaks the x axis of plots.
#' @return A list of ggplot objects
#' @export
#'
#' @examples
#' \dontrun{
#'
#' load("data/adsl.rda")
#'
#' sl_summary(adsl,
#'           vars = "SEX",
#'           kinds = "bar_binom",
#'           trt = "TRT01P")
#'
#' sl_summary(adsl,
#'           vars = "SEX",
#'           kinds = "bar_binom",
#'           titles = "Female Subjects (%)",
#'           trt = "TRT01P")
#'
#' sl_summary(adsl,
#'           vars = list("SEX" = "M"),  # using `list()` style only works for `bar_binom` kind
#'           kinds = "bar_binom",
#'           titles = "Male Subjects (%)",
#'           trt = "TRT01P")
#'
#'
#' plots <- sl_summary(adsl, vars = c(list("SEX" = "M"), "SEX"),
#'           kinds = c("bar_binom"),
#'           trt = "TRT01P",
#'           show_plots = TRUE,
#'           ncol = 2)
#'
#' plots <- sl_summary(adsl,
#'             vars = c("RACE", "SEX"),
#'             kinds = c("bar_stack"),        # `bar_stack` visualization will be applied for both `SEX` and `RACE`
#'             titles = c("RACE (%)", NA),    # Put `NA` when you don't need to have a main title for your plot
#'             trt = "TRT01P")
#'
#' sl_summary(adsl,
#'            vars = "AGEGR1",
#'            relevel_vec = list(adsl %>% pull(AGEGR1) %>% levels() %>% rev()),
#'            kinds = "bar_stack",        # `bar_stack` visualization will be applied for both `SEX` and `RACE`
#'            titles = "AGE Group 1 (%)",  # Put `NA` when you don't need to have a main title for your plot
#'            trt = "TRT01P")
#'
#' plots <- sl_summary(adsl,
#'             vars = c("SEX", "AGE"),
#'             kinds = c("bar_stack", "box"),
#'             titles = c("Female Subjects (%)", "AGE Groups"),
#'             trt = "TRT01A",
#'             show_plots = FALSE)
#'
#' plots[[1]]
#' patchwork::wrap_plots(plots, ncol = 1)
#'
#' ggplot2::ggsave(
#'        plots[[1]],
#'        filename = here::here("Your_File", "Figure01_sex_f.png"),
#'        width = 4,
#'        height = 2
#'      )
#'
#' }
sl_summary <- function(data, vars, kinds, trt = "TRT01P", titles = NULL, show_plots = TRUE, ncol = 1, relevel_vec = list(NULL), breaks_vec = list(NULL), base_size = 18, ...){


  check_length(vars, kinds)
  check_length(vars, titles)

  if(length(kinds) == 1){
    kinds = rep(kinds, length(vars))
  }

  if(length(titles) == 1){
    titles = rep(titles, length(vars))
  }

  if(length(trt) == 1){
    trt = rep(trt, length(vars))
  }

  if(length(relevel_vec) == 1){
    relevel_vec = rep(relevel_vec, length(vars))
  }

  if(length(breaks_vec) == 1){
    breaks_vec = rep(breaks_vec, length(vars))
  }

  output_objects <- rep(list(NA), length(vars))


  for(i in 1:length(vars)){

    if(kinds[i] == 'box'){
      output_objects[[i]] <- get_box(data, var = vars[i], trt = trt[i], title = titles[i], breaks_vec = breaks_vec[[i]], base_size)
    } else if(kinds[i] == 'bar_binom'){
      output_objects[[i]] <- get_bar_binom(data, var = vars[i], trt = trt[i], title = titles[i], base_size)
    } else if(kinds[i] == 'bar_stack'){
      output_objects[[i]] <- get_bar_stack(data, var = vars[i], trt = trt[i], title = titles[i], base_size, relevel_vec = relevel_vec[[i]])
    }

  }

  output <- if(length(vars) == 1){
    output_objects[[1]]
  } else {
    output_objects
  }

  if(show_plots){
    return(patchwork::wrap_plots(output, ncol = ncol))
  }

  return(output)

}



# utilities =================================================================================


#' Check the length of variables and kinds in the \code{sl_summary()} functions are matched
#' @keywords internal
#' @noRd
check_length <- function(vars, alts){
  if(!is.null(alts) && length(alts)!= 1 && (length(alts) != length(vars))){
    stop(glue::glue("`{alts}` should have length 1 or same as length(vars)."))
  } else {
    return(invisible(NULL))
  }
}



#' Build a ggplot boxplot object for a continuous variable
#'
#' @param data dataframe with variables to plot. It generally is a subject level data (`adsl`).
#' @param var the baseline variable of interest to display
#' @param trt the column name for planned treatment. Default is `TRT01P`.
#' @param title the main title of plot.
#' @param breaks_vec the vector of breaks showing in the x axis.
#'
#' @return ggplot object
#' @keywords internal
#' @noRd
get_box <- function(data, var, trt, title, breaks_vec, base_size = 18) {

  ## Get the variable label for plot axis
  #------------------------------------------------
  all <- sjlabelled::get_label(data)
  #selection <- deparse(substitute(var))
  selection <- var
  xlab <- all[[selection]]

  data %>%
    dplyr::group_by(!!sym(trt), !!sym(var)) %>%
    dplyr::summarize(N = dplyr::n()) %>%
    dplyr::mutate(
      freq = N / sum(N),
      pct = round((freq * 100), 1),
      axis_lab = pct #paste0(pct, "%")
    ) %>%
    group_by(!!sym(trt)) %>%
    mutate(nTRT = sum(N)) %>%
    ungroup() %>%
    # dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", substr(!!sym(trt),1, 1) %>% toupper(), "=", nTRT,")")) %>%
    dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", "=", nTRT,")")) %>%
    dplyr::ungroup()


  data_added_labels <- data %>%
    dplyr::group_by(!!sym(trt)) %>%
    dplyr::summarise(nTRT = dplyr::n()) %>%
    dplyr::ungroup() %>%
    #dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", substr(!!sym(trt),1, 1) %>% toupper(), "=", nTRT,")"))
    dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", "=", nTRT,")"))


  gg <- data %>%
    select(!!sym(trt), !!sym(var)) %>%
    left_join(data_added_labels %>% select(!!sym(trt), trt_with_label)) %>%
    ggplot(aes(x = !!sym(var),
               y = trt_with_label )) +
    geom_boxplot(alpha = 0.9, width = 0.4) +
    stat_summary(
      fun.y = "mean",
      geom = "point",
      shape = 20,
      size = 4,
      color = "red",
      fill = "white",
      alpha = 0.5
    ) +
    #geom_rug(sides = "b") +
    #theme_light(base_size = 10) +
    theme_minimal(base_size = base_size) +
    # scale_x_continuous(limit = c(min(data[[selection]]), max(data[[selection]])),
    #                    breaks = round(fivenum(data[[selection]]), 0)) +
    xlab(xlab) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      plot.title = element_text(hjust = 0.1)
    )

  if(is.null(title)){
    gg <- gg +
          ggtitle(xlab)
  } else {
    gg <- gg +
      ggtitle(title)
  }


  if(is.null(breaks_vec)){
    gg <- gg +
      scale_x_continuous(limit = c(min(data[[selection]]), max(data[[selection]])),
                         breaks = round(fivenum(data[[selection]]), 0))
  } else {
    gg <- gg +
      scale_x_continuous(breaks = breaks_vec)
  }




  return(gg)

}




# sl_summary(adsl,
#            vars = "SEX",
#            kinds = "bar_binom",
#            titles = "Female Subjects (%)",
#            trt = "TRT01P")


#' Build a ggplot bar plot object for a binomial variable
#'
#' @param data dataframe with variables to plot. It generally is a subject level data (`adsl`).
#' @param var the baseline variable of interest to display
#' @param trt the column name for planned treatment. Default is `TRT01P`.
#' @param title the main title of the ggplot
#' @param base_size the \code{base_size} of the ggplot \code{theme_minimal}. Default is 11.
#'
#' @return ggplot object
#' @keywords internal
#' @noRd
get_bar_binom <- function(data, var, trt, title = NA, base_size = 18){

  if(class(var) == 'list' && names(var) != ""){
    var_filter = var[[1]]
    var = names(var)
  } else {
    var <- var %>% unlist()
    var_filter = (data %>% dplyr::count(!!sym(var), sort = T))[1,1] %>% as.character()
  }

  title = if(is.null(title) || is.na(title)){
    glue::glue("{var_filter} subjects (%)")
  } else{
    title
  }


  if(data %>% dplyr::pull(!!sym(var)) %>% unique() %>% length() != 2){
    stop(glue::glue("{var} is not a binary variable."))
  }

  if(is.na(title)){
    title <- NULL
  }


  data %>%
    dplyr::group_by(!!sym(trt), !!sym(var)) %>%
    dplyr::summarize(N = dplyr::n()) %>%
    #ungroup() %>%
    dplyr::mutate(
      freq = N / sum(N),
      pct = round((freq * 100), 1)
    ) %>%
    dplyr::mutate(axis_lab = paste0(pct)) %>%
    group_by(!!sym(trt)) %>%
    mutate(nTRT = sum(N)) %>%
    ungroup() %>%
    #dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", substr(!!sym(trt),1, 1) %>% toupper(), "=", nTRT,")")) %>%
    dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", "=", nTRT,")")) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym(var) == var_filter) %>%
    ggplot(aes(x = trt_with_label, y = pct, fill = !!sym(var), label = axis_lab)) +
    geom_hline(yintercept = 0, colour = "wheat4", linetype=1, size=0.6)+
    geom_bar(stat = "identity", size = 0.5, position = position_dodge(width = 1), alpha = 0.8) +
    geom_text(aes(y = pct), position = position_dodge(width = 1), size = 5, vjust = 0, hjust=1.6, color = "white") +
    scale_y_continuous(limit = c(0, 100))+
    theme_minimal(base_size = base_size) +
    coord_flip() +
    xlab("")+
    # ylab("Percentage of subjects (%)") +
    labs(title = title) +
    scale_fill_grey() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.1))

}


#' Build a ggplot stacked bar plot object for a binomial or multinomial variable
#'
#' @param data dataframe with variables to plot. It generally is a subject level data (`adsl`).
#' @param var the baseline variable of interest to display
#' @param trt the column name for planned treatment. Default is `TRT01P`.
#' @param title the main title of the ggplot
#' @param base_size the \code{base_size} of the ggplot \code{theme_minimal}. Default is 10.
#' @param relevel_vec A vector of characters that shoes a relevel of the `var` levels. It affect the order
#'   of stacked bar plots.
#'
#' @return ggplot object
#' @keywords internal
#' @noRd
get_bar_stack <- function(data, var, trt, title = NA, base_size = 10, relevel_vec = NULL){

  if(is.na(title)){
    title <- NULL
  }

  if(!is.null(relevel_vec)){
    data <- data %>%
      dplyr::mutate(!!sym(var) := factor(!!sym(var), levels = relevel_vec))
  }


  plot <- data %>%
    dplyr::group_by(!!sym(trt), !!sym(var)) %>%
    dplyr::summarize(N = dplyr::n()) %>%
    dplyr::mutate(
      freq = N / sum(N),
      pct = round((freq * 100), 1),
      axis_lab = pct #paste0(pct, "%")
      ) %>%
    group_by(!!sym(trt)) %>%
    mutate(nTRT = sum(N)) %>%
    ungroup() %>%
    #dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", substr(!!sym(trt),1, 1) %>% toupper(), "=", nTRT,")")) %>%
    dplyr::mutate(trt_with_label =  paste0(!!sym(trt), "\n", "(n", "=", nTRT,")")) %>%
    dplyr::ungroup() %>%
    ggplot(aes(x = trt_with_label, y = pct, fill = !!sym(var), label = axis_lab, order = !!sym(var))) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(limit = c(0, 100))+
    theme_minimal(base_size = base_size) +
    coord_flip() +
    xlab("")+
    ylab("Percentage of subjects (%)") +
    labs(title = title, fill = "") +
    scale_fill_brewer(direction = -1) +
    theme(legend.position = "right",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5))

  plot

}



