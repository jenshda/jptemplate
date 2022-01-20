#' Insert R Project Template
#'
#' This function will insert the R project template into the current R project template folder
#'
#' * title        = 'the name of the project' \n
#' * subtitle     = 'the subtitle of the project' \n
#' * gitignore    = Set to TRUE if you want to ignore the gitignore file \n
#' * readme     = FALSE disables the creation of a template readme file \n
#' * rmarkdown  = FALSE disables the creation of a template rmarkdown file \n
#' * folders    = FALSE disables the creation of folders \n
#'
#' It's callback is at: inst/rstudio/templates/project/create_project.dcf
#'
#' @export
create_project <-
  function(title, subtitle = NULL, gitignore = FALSE, readme = TRUE, rmarkdown = TRUE, folders = TRUE) {

    # In the project template we've added 2 choices for the user:
    # * One allows them to select if the project will have a .gitignore file

    # Check .gitignore argument
    if(gitignore == "TRUE") {
      git_ignores <-
        c(
          '.Rhistory',
          '.Rapp.history',
          '.RData',
          '.Ruserdata',
          '.Rproj.user/',
          '.Renviron'
        )
      writeLines(paste(git_ignores, sep = '\n'), '.gitignore')
    }

    if(readme == "TRUE"){
      writeLines(c(
        "",
        "#       README FILE ",
        "---",
        "- `Author:` Jens Halford Dyster-Aas",
        paste0("- `Copyright:` (c) Jens Halford Dyster-Aas ", "(", format(Sys.Date(), '%Y'), ")"),
        "- `Email:`  jens.halford@gmail.com",
        paste0("- `Script Name:` ", title, ".r"),
        paste0("- `Date:` ", format(Sys.Date(), '%B %d, %Y')),
        "---",
        "",
        "**Script Description**:",
        "",
        "",
        "**Notes**:",
        "",
        "",
        "---"
        ), "README.md")
     }


    if(rmarkdown == "TRUE"){

      insert <- function(title, subtitle){
        if(missing(subtitle)){
          writeLines(c(
            paste0("title: ", title)))
        } else {
          writeLines(c(
            paste0("title: ", title),
            paste0("subtitle: ", subtitle)
          )
          )
        }}

      writeLines(c(
        "---",
        "author: Jens Halford Dyster-Aas",
        if(!is.null(subtitle)) {
          insert(title, subtitle)
        } else {
          insert(title)
        },
        paste0("copyright: (c) Jens Halford Dyster-Aas ", "(", format(Sys.Date(), '%Y'), ")"),
        "email:  jens.halford@gmail.com",
        paste0("title:", title, ".r"),
        paste0("date:", format(Sys.Date(), '%B %d, %Y')),
        "output: ",
        " html_notebook: ",
        "  toc:             true",
        "  toc_float:       false",
        "  fig_caption:     true",
        "  highlight:       tango",
        "  code_folding:    hide",
        "  df_print:        paged",
        "  section_divs:    true",
        "  number_sections: true",
        "  theme:           cosmo",
        "---",
        "",
        "---",
        "",
        "**Description:** ",
        "",
        "---",
        "",
        "## Load Packages",
        "```{r Load Packages, echo = FALSE, warning = TRUE, message = TRUE}",
        "",
        "```",
        "",
        "##",
        "```{r Title, echo = FALSE, warning = TRUE, message = TRUE}",
        "",
        "```"
      ), paste0(title, ".Rmd"))
    }

    if (folders == "TRUE"){
    # Check selected folder
    dir.create("Data",               recursive = TRUE, showWarnings = FALSE)
    dir.create("Data/Processed",     recursive = TRUE, showWarnings = FALSE)
    dir.create("Data/Raw",           recursive = TRUE, showWarnings = FALSE)
    dir.create("Data/Final",         recursive = TRUE, showWarnings = FALSE)
    dir.create("Markdown",           recursive = TRUE, showWarnings = FALSE)
    dir.create("Output",             recursive = TRUE, showWarnings = FALSE)
    dir.create("Output/Plots",       recursive = TRUE, showWarnings = FALSE)
    dir.create("Output/Tables",      recursive = TRUE, showWarnings = FALSE)
    dir.create("Scripts",            recursive = TRUE, showWarnings = FALSE)
    dir.create("Scripts/Analysis",   recursive = TRUE, showWarnings = FALSE)
    dir.create("Scripts/Modelling",  recursive = TRUE, showWarnings = FALSE)
    dir.create("Scripts/Production", recursive = TRUE, showWarnings = FALSE)
    dir.create("lib",                recursive = TRUE, showWarnings = FALSE)
    }
  }


# devtools::document()  function to create its documentation pages.
                      # Even though this package is mainly developed to be an add-in for RStudio,
                      # it's a good practice to document the package, before installing it in R,
                      # since it will search for any dependencies and packages that are needed for your project to work.
# devtools::install()   install the package and make it available in any other R Session from now on.
                      # This should be enough to make the add-in accessible by RStudio as a New Project Template.
