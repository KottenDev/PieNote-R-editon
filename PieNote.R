library(tcltk)

current_file_path <- NULL

new_file <- function() {
  current_file_path <<- NULL
  tkdelete(txt, "1.0", "end")
  tkwm.title(root, "Untitled - PieNote")
}

open_file <- function() {
  file_path <- as.character(tk_getOpenFile(
    filetypes = "{{Text Files} {.txt}} {{All Files} {*}}"
  ))

  if (nzchar(file_path)) {
    current_file_path <<- file_path
    content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")

    tkdelete(txt, "1.0", "end")
    tkinsert(txt, "1.0", content)

    tkwm.title(root, paste0(file_path, " - PieNote"))
  }
}

save_file <- function() {
  if (!is.null(current_file_path)) {
    tryCatch({
      file_content <- as.character(tkget(txt, "1.0", "end"))
      cat(file_content, file = current_file_path, sep = "")
      tk_messageBox(type = "ok", message = "File saved successfully!",
                    icon = "info", title = "Saved")
    }, error = function(e) {
      tk_messageBox(type = "ok",
                    message = paste0("Failed to save file: ", conditionMessage(e)),
                    icon = "error", title = "Error")
    })
  } else {
    save_file_as()
  }
}

save_file_as <- function() {
  file_path <- as.character(tk_getSaveFile(
    defaultextension = ".txt",
    filetypes = "{{Text Files} {.txt}} {{All Files} {*}}"
  ))

  if (nzchar(file_path)) {
    current_file_path <<- file_path
    file_content <- as.character(tkget(txt, "1.0", "end"))

    cat(file_content, file = file_path, sep = "")

    tkwm.title(root, paste0(file_path, " - PieNote"))
    tk_messageBox(type = "ok", message = "File saved successfully!",
                  icon = "info", title = "Saved")
  }
}

open_about <- function() {
  about_window <- tktoplevel(root)
  tkwm.title(about_window, "About")
  window_width <- 250
  window_height <- 150

  root_width <- as.integer(tkwinfo("width", root))
  root_height <- as.integer(tkwinfo("height", root))
  root_x <- as.integer(tkwinfo("x", root))
  root_y <- as.integer(tkwinfo("y", root))

  center_x <- as.integer(root_x + (root_width / 2) - (window_width / 2))
  center_y <- as.integer(root_y + (root_height / 2) - (window_height / 2))

  tkwm.geometry(about_window,
                paste0(window_width, "x", window_height, "+", center_x, "+", center_y))

  tkwm.transient(about_window, root)
  tkgrab.set(about_window)

  label <- tklabel(about_window,
                   text = "PieNote\nR edition\nBuilt By kotten.",
                   pady = 20)
  tkpack(label)
}

root <- tktoplevel()
tkwm.title(root, "PieNote")

menu <- tkmenu(root)
tkconfigure(root, menu = menu)

filemenu <- tkmenu(menu, tearoff = 0)
tkadd(menu, "cascade", label = "File", menu = filemenu)
tkadd(filemenu, "command", label = "New", command = new_file)
tkadd(filemenu, "command", label = "Open...", command = open_file)
tkadd(filemenu, "command", label = "Save", command = save_file)
tkadd(filemenu, "command", label = "Save As...", command = save_file_as)
tkadd(filemenu, "separator")
tkadd(filemenu, "command", label = "Exit", command = function() tkdestroy(root))

helpmenu <- tkmenu(menu, tearoff = 0)
tkadd(menu, "cascade", label = "Help", menu = helpmenu)
tkadd(helpmenu, "command", label = "About", command = open_about)

txt <- tktext(root, height = 15, width = 50)
tkpack(txt, padx = 10, pady = 10, expand = TRUE, fill = "both")

app_icon <- tkimage.create("photo", file = "PieNote.png")
tcl("wm", "iconphoto", root, "-default", app_icon)