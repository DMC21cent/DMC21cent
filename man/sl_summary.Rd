% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_sl_summary.R
\name{sl_summary}
\alias{sl_summary}
\title{A subject level graphical summary}
\usage{
sl_summary(
  data,
  vars,
  kinds,
  trt = "TRT01P",
  titles = NULL,
  show_plots = TRUE,
  ncol = 1,
  relevel_vec = list(NULL),
  breaks_vec = list(NULL),
  base_size = 18,
  ...
)
}
\arguments{
\item{data}{dataset with variables to plot. It generally is a subject level data (`adsl`).}

\item{vars}{the vector of baseline variables of interest to display.}

\item{kinds}{the type of graphical visualization corresponding to each element of `vars`.
It may be a length one string means that graph will be used for all the varialbes of `vars`.
Otherwise, `kinds` should have the same length of `vars`. In this case, each element of `kinds`, will
be applied to its corresponding element of `vars`. `kinds` may take three options for each element:
\itemize{
  \item{"box"}{special box plot, combining with rug plot, for `continuous` variables}
  \item{"bar_binom"}{bar plot for categorical variables. It shows only one of the levels.
     By default, it selects the high frequency level.
     For selecting the other level, you can follow the format
     of `list("var" = "level")` in the `vars`. Check the examples to see how it works}
  \item{"bar_stack"}{The stack bar chart for binomial or multinomial variables.}
 }}

\item{trt}{the column name for planned treatment. Default is `TRT01P`.}

\item{titles}{the vector of main titles for each graph. By default all is `NA` which means no title.}

\item{show_plots}{logical If `TRUE`, it combined all the plots and shows them using \code{\link[patchwork]{wrap_plots}}.
For controlling the number of columns, check `ncol`}

\item{ncol}{numeric. If `show_plots` is `TRUE`, `ncol` controls the number of column. Default is 1.}

\item{breaks_vec}{A list of vectors that breaks the x axis of plots.}

\item{relevel_list}{A list of vectors that applies a relevel on the `var` levels. It affect the order
of stacked bar plots. It only affects `bar_stack` kinds.}
}
\value{
A list of ggplot objects
}
\description{
Some systematic visualizations to display the characteristics of the study population
}
\examples{
\dontrun{

load("data/adsl.rda")

sl_summary(adsl,
          vars = "SEX",
          kinds = "bar_binom",
          trt = "TRT01P")

sl_summary(adsl,
          vars = "SEX",
          kinds = "bar_binom",
          titles = "Female Subjects (\%)",
          trt = "TRT01P")

sl_summary(adsl,
          vars = list("SEX" = "M"),  # using `list()` style only works for `bar_binom` kind
          kinds = "bar_binom",
          titles = "Male Subjects (\%)",
          trt = "TRT01P")


plots <- sl_summary(adsl, vars = c(list("SEX" = "M"), "SEX"),
          kinds = c("bar_binom"),
          trt = "TRT01P",
          show_plots = TRUE,
          ncol = 2)

plots <- sl_summary(adsl,
            vars = c("RACE", "SEX"),
            kinds = c("bar_stack"),        # `bar_stack` visualization will be applied for both `SEX` and `RACE`
            titles = c("RACE (\%)", NA),    # Put `NA` when you don't need to have a main title for your plot
            trt = "TRT01P")

sl_summary(adsl,
           vars = "AGEGR1",
           relevel_vec = list(adsl \%>\% pull(AGEGR1) \%>\% levels() \%>\% rev()),
           kinds = "bar_stack",        # `bar_stack` visualization will be applied for both `SEX` and `RACE`
           titles = "AGE Group 1 (\%)",  # Put `NA` when you don't need to have a main title for your plot
           trt = "TRT01P")

plots <- sl_summary(adsl,
            vars = c("SEX", "AGE"),
            kinds = c("bar_stack", "box"),
            titles = c("Female Subjects (\%)", "AGE Groups"),
            trt = "TRT01A",
            show_plots = FALSE)

plots[[1]]
patchwork::wrap_plots(plots, ncol = 1)

ggplot2::ggsave(
       plots[[1]],
       filename = here::here("Your_File", "Figure01_sex_f.png"),
       width = 4,
       height = 2
     )

}
}
