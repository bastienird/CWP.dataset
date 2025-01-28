#' Save Plot as Image
#'
#' @description This function saves the current plot as an image in a specified folder.
#' @param title A character string representing the title of the plot.
#' @param plott The plot object to save.
#' @param folder The folder where the image will be saved.
#' @param fig.pathinside The path for saving the figure.
#' @param find_and_print Logical indicating if results should be printed.
#' @return None
#' @export
save_image <- function(title, plott = last_plot(), folder = NULL, fig.pathinside = fig.path, find_and_print = FALSE){
  current <- tmap_mode()
  title <- eval(title)
  if(!is.null(folder)){
    dir.create(file.path(fig.pathinside, folder), recursive = TRUE)

    if(all(class(plott) == "flextable")){
      save_as_image(plott,path = file.path(fig.pathinside, file.path(folder, paste0( make.names(title), ".png"))))
    } else if(all(class(plott) == "tmap")){
      tmap_mode("plot")
      filenametmap <- file.path(fig.pathinside, file.path(folder, paste0( make.names(title), ".png")))
      tmap_save(tm = plott, filename = filenametmap)
      tmap_mode(current)
    } else {
      ggsave(paste0( make.names(title), ".png"),plot = plott,   device = "png", path = file.path(fig.pathinside, folder), create.dir = TRUE)
    }
  } else { print("Cannot save the image the folder does not exist")}
  # return(plott)

}
