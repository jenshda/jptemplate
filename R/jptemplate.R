#' Insert R Project Template
#'
#' This function will insert the R project template into the current R project
#' template folder including a default set of sub-folders, a \code{README.md} template file,
#'  and the default \code{.Rmd} file. All files will automatically contain the project name
#'  used in the function.
#'
#'
#'
#' @param title        a mandatory character input of 'the name of the project'.
#' @param subtitle     an optional character input of 'the subtitle of the project'.
#' @param author       an optional character input of 'the project author'.
#' @param gitignore    logical. Set to \code{TRUE} if you want to ignore the \code{.gitignore} file.
#' @param readme       logical. \code{FALSE} disables the creation of a template \code{README.md} file.
#' @param rmarkdown    logical. \code{FALSE} disables the creation of a template \code{.Rmd} file.
#' @param folders      logical. \code{FALSE} disables the creation of folders.
#'
#'
#' @examples create_project("Project_Name", subtitle = "Project_Subtitle", gitignore = TRUE, rmarkdown = FALSE)
#'
#' @export
create_project <-
  function(title, subtitle = NULL, author = NULL, gitignore = FALSE, readme = TRUE, rmarkdown = TRUE, folders = TRUE) {

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

      insert2 <- function(author){
        if(missing(author)){
          ""
        } else {writeLines(c(author))
        }
      }
      author_n <- capture.output(if(!is.null(author)) {
        insert2(author)
      } else {
        cat("NO_AUTHOR")
      })

      writeLines(c(
        "",
        "#       README FILE ",
        "---",
        paste0("- `Author:` ", author_n),
        paste0("- `Copyright:` (c) ", author_n, " (", format(Sys.Date(), '%Y'), ")"),
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

  # Function for title and subtitle
  insert <- function(title, subtitle){
    if(missing(subtitle)){
      writeLines(c(
        paste0("title: ", title)))
    } else {
      writeLines(c(
        paste0("title: ", title),
        paste0("subtitle: ", subtitle)))
    }}
    title_n <- capture.output(if(!is.null(subtitle)) {
      insert(title, subtitle)
    } else {
      insert(title)
    })

    # Function for author
    insert2 <- function(author){
      if(missing(author)){
        ""
      } else {writeLines(c(author))
      }
    }
    author_n <- capture.output(if(!is.null(author)) {
      insert2(author)
    } else {
      cat("NO_AUTHOR")
    })

  writeLines(c(
    "---",
    title_n,
    paste0("author: ", author_n),
    paste0("copyright: (c) ", author_n, " (", format(Sys.Date(), '%Y'), ")"),
    paste0("filename: ", title, ".Rmd"),
    paste0("date: ", format(Sys.Date(), '%B %d, %Y')),
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
    "# Project Setup",
    "```{r Load Packages, results =FALSE, echo = TRUE}",
    "",
    "```",
    "```{r Prepare Project,  results =FALSE, echo = FALSE}",
    "",
    " # Set up the input and output folders with relative paths",
    paste0(" # Set the project folder for \"", title, "\""),
    "  data.folder        <- file.path(\"Data\")",
    "    raw.folder       <- file.path(data.folder,    \"Raw Datasets\")",
    "    manip.folder     <- file.path(data.folder,    \"Manipulated Datasets\")",
    "    final.folder     <- file.path(data.folder,    \"Final Datasets\")",
    "  scripts.folder     <- file.path(\"Scripts\")",
    "    analysis.folder  <- file.path(scripts.folder, \"Analysis\")",
    "    functions.folder <- file.path(scripts.folder, \"Functions\")",
    "    modelling.folder <- file.path(scripts.folder, \"Modelling\")",
    "  output.folder      <- file.path(\"Output\")",
    "    plot.folder      <- file.path(output.folder,  \"Data\")",
    "    table.folder     <- file.path(output.folder,  \"Data\")",
    "    markdown.folder  <- file.path(output.folder,  \"Data\")",
    "  library.folder     <- file.path(\"lib\")",
    "",
    "  # Example",
    "  # Create a file in the functions folder",
    "  # file.create(file.path(functions.folder, \"functions.r\"))",
    "",
    "```",
    "",
    "```{r Load functions,  results =FALSE, echo = FALSE}",
    "  # Run all functions",
    "  list.files(functions.folder, full.names = TRUE) %>% map(source)",
    "```",
    "",
    "##",
    "```{r Title, results = FALSE, echo = TRUE}",
    "",
    "",
    "```"
  ), paste0(title, ".Rmd"))
}

if (folders == "TRUE"){
  # Check selected folder
  ifelse(!dir.exists("Data/Processed Datasets"),
         dir.create("Data/Processed Datasets", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Data/Raw Datasets"),
         dir.create("Data/Raw Datasets", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Data/Final Datasets"),
         dir.create("Data/Final Datasets", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Output/Plots"),
         dir.create("Output/Plots", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Output/Tables"),
         dir.create("Output/Tables", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Output/Markdown"),
         dir.create("Output/Markdown", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Scripts/Analysis"),
         dir.create("Scripts/Analysis", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Scripts/Modelling"),
         dir.create("Scripts/Modelling", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("Scripts/Functions"),
         dir.create("Scripts/Functions", recursive = TRUE),
         "Folder exists already")
  ifelse(!dir.exists("lib"),
         dir.create("lib", recursive = TRUE), "Folder exists already")
}
}

# cmd+shift+d function to create its documentation pages.
# cmd+shift+b install the package and make it available in any other R Session from now on.
# cmd+shift+e evaluate package


