## THE SERVER OF THE APP -------------------------------------------------------

#' Settings that belong to the data (rows, columns, clusters or values of it):
#' reset when the data changes
#' @noRd
.APP_DATA_SPECIFIC <- c(
  "tree_x", "tree_y", "tree_cut_x", "tree_cut_y", "tree_kmeans_x",
  "tree_kmeans_y", "tree_col_x", "tree_col_y", "tree_support_x",
  "tree_support_y", "tree_label_text_x", "tree_label_text_y", "tree_split_x",
  "mark_x", "mark_y", "mark_labels_x", "mark_labels_y", "zoom_x", "zoom_y",
  "bar_values_x", "bar_values_y", "annotation_x", "annotation_y",
  "annotation_colours", "axis_text_x", "axis_text_y", "cell_split",
  "cell_split_size", "cell_uncertainty", "cell_values", "cell_border_mask",
  "cell_col_scale_limits", "cell_col_scale_breaks", "cell_col_blocks_x",
  "cell_col_blocks_y", "legend_col_breaks", "cell_size_scale_limits"
)

#' The most columns a table can have for the app to list a role for each
#' @noRd
.APP_ROLE_COLUMNS <- 40

#' What each column of a table does at first: show (cells of the heatmap),
#' group or annotate the rows, or leave out
#' @noRd
.app_default_roles <- function(d) {
  x <- d$x
  cols <- colnames(x)
  vapply(seq_along(cols), function(i) {
    v <- if(is.data.frame(x)) x[[i]] else x[, i]
    if(cols[i] %in% d$group) return("group")
    if(is.numeric(v)) return("show")
    n <- length(unique(v))
    if(n <= 30) "annotate" else "leave"
  }, character(1))
}

#' The roles a column can take, named by their labels
#' @noRd
.app_role_choices <- function(d,
                              i) {
  x <- d$x
  v <- if(is.data.frame(x)) x[[i]] else x[, i]
  col <- colnames(x)[i]
  out <- c(if(is.numeric(v)) "Show" else "Show as categories")
  vals <- "show"
  if(col %in% d$meta) {
    out <- c(out, "Group the rows")
    vals <- c(vals, "group")
  }
  stats::setNames(c(vals, "annotate", "leave"),
                  c(out, "Annotate the rows", "Leave out"))
}

#' A short description of a column: its kind and range, or its values
#' @noRd
.app_column_text <- function(v) {
  if(is.numeric(v)) {
    r <- range(v, na.rm = TRUE)
    n <- length(unique(v[!is.na(v)]))
    if(n <= 12) {
      return(paste0("Numbers, ", n, " values"))
    }
    return(paste0("Numbers, ", signif(r[1], 3), " to ", signif(r[2], 3)))
  }
  n <- length(unique(v))
  paste0(if(is.logical(v)) "TRUE or FALSE" else "Text", ", ", n,
         if(n == 1) " value" else " values")
}

#' Whether a table lists a role for each of its columns
#' @noRd
.app_roles_mode <- function(d) {
  !is.null(d) && d$kind == "table" && length(d$features) <= .APP_ROLE_COLUMNS
}

#' The sentence the app writes about the colour-vision check of a heatmap
#' @noRd
.app_colour_text <- function(hm) {
  chk <- tryCatch(suppressMessages(heat_map_check_colours(hm)),
                  error = function(e) NULL)
  if(is.null(chk)) {
    return("")
  }
  and <- function(x) {
    if(length(x) < 2) x else paste(paste(x[-length(x)], collapse = ", "),
                                   "and", x[length(x)])
  }
  vision <- attr(chk, "deficiency")
  if(nrow(chk) == 0) {
    return(paste0("The colours stay apart with ", and(vision), "."))
  }
  n <- nrow(chk)
  paste0(n, if(n == 1) " pair" else " pairs",
         " of colours are hard to tell apart with ",
         and(unique(chk$vision)), ", e.g. ", chk$first[1], " and ",
         chk$second[1], ".")
}

#' The preview of a heatmap: the image and its rows, columns and values
#' @noRd
.app_preview <- function(hm,
                         width,
                         height) {
  # (TWICE THE PIXELS OF THE SCREEN FOR SHARP TEXT, AT MOST 4,000 WIDE)
  res <- min(192, floor(4000 / max(width, height)))
  drawn <- .html_draw(hm, list(), width, height, "png", res)
  payload <- suppressMessages(.html_payload(drawn, 2e5))
  list(image = drawn$image, data = .to_json(payload), width = 96 * width,
       height = 96 * height)
}

#' The histogram of the values of a heatmap under the limits setting, with
#' the values beyond the limits greyed
#' @noRd
.app_histogram <- function(values,
                           limits = NULL) {
  values <- values[is.finite(values)]
  if(length(values) < 2 || diff(range(values)) == 0) {
    return(NULL)
  }
  breaks <- seq(min(values), max(values), length.out = 25)
  counts <- graphics::hist(values, breaks = breaks, plot = FALSE)$counts
  mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  out <- if(!is.null(limits) && length(limits) == 2) {
    mids < limits[1] | mids > limits[2]
  } else rep(FALSE, length(mids))
  top <- max(counts)
  shiny::div(
    class = "hm-hist",
    shiny::div(
      class = "hm-hist-bars", `aria-hidden` = "true",
      lapply(seq_along(counts), function(i) {
        shiny::span(class = if(out[i]) "out" else NULL,
                    style = paste0("height:", max(2, round(56 * counts[i] / top)),
                                   "px"))
      })
    ),
    shiny::div(class = "hm-hist-axis",
               shiny::span(signif(min(values), 3)),
               shiny::span(signif(max(values), 3))),
    shiny::p(class = "hm-hint",
             "The values as drawn. Values beyond the limits (grey) take the colours at the ends of the scale.")
  )
}

#' The columns of the data's metadata with two values (e.g. two conditions),
#' which can split the dots of a dot plot
#' @noRd
.app_two_values <- function(d) {
  if(is.null(d) || length(d$meta) == 0) {
    return(character(0))
  }
  meta <- if(d$kind == "cells") .cell_source(d$x)$meta else d$x
  if(is.null(meta)) {
    return(character(0))
  }
  Filter(function(nm) {
    v <- meta[[nm]]
    !is.null(v) && length(unique(v[!is.na(v)])) == 2
  }, d$meta)
}

#' What a selection on the heatmap keeps: the features among its labels, the
#' rows of a table (not of a dot plot, whose rows are groups) and the groups
#' of a dot plot grouped by one column
#'
#' @param d the data.
#' @param labels the labels selected (rows and columns).
#' @param fun the function drawing the heatmap.
#' @param data_code the R code of the arguments choosing the data.
#' @return a list: features, rows (names) and groups (a list of the column
#'   and its values, or NULL).
#' @noRd
.app_selection <- function(d,
                           labels,
                           fun,
                           data_code) {
  labels <- as.character(labels)
  out <- list(features = intersect(labels, d$features), rows = character(0),
              groups = NULL)
  group <- if("group_by" %in% names(data_code)) {
    eval(parse(text = data_code[["group_by"]])[[1]], baseenv())
  }
  if(fun == "heat_map_dot") {
    if(length(group) == 1 && d$kind == "table") {
      values <- unique(as.character(d$x[[group]]))
      hit <- intersect(labels, values)
      if(length(hit) > 0) out$groups <- list(column = group, values = hit)
    }
  } else if(d$kind == "table" && fun == "heat_map" && !is.null(rownames(d$x))) {
    out$rows <- setdiff(intersect(labels, rownames(d$x)), out$features)
  }
  out
}

#' The download dialog
#' @noRd
.app_download_modal <- function(image,
                                name) {
  formats <- list(
    c("png", "PNG", "Slides and web"), c("pdf", "PDF", "Print, vector"),
    c("svg", "SVG", "Edit in Inkscape"), c("tiff", "TIFF", "Journals, lossless"),
    c("jpeg", "JPEG", "Small files")
  )
  px <- paste0(round(image$width * image$res), " &times; ",
               round(image$height * image$res), " pixels")
  shiny::modalDialog(
    title = "Download", size = "l", easyClose = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::downloadButton("code_download_modal", "Save the R script",
                            class = "hm-btn", icon = NULL),
      shiny::downloadButton("download", "Download the image",
                            class = "hm-btn hm-primary", icon = NULL)
    ),
    shiny::div(
      class = "hm-dl",
      shiny::div(
        class = "hm-dl-size",
        shiny::strong(shiny::HTML(paste0(image$width, " &times; ",
                                         image$height, " in"))),
        shiny::p(shiny::HTML(paste0(
          "At ", image$res, " dpi: ", px, ". ",
          "Choose the size above the heatmap; text keeps its size ",
          "and the heatmap takes the space."
        )))
      ),
      shiny::div(
        class = "hm-formats",
        shiny::radioButtons(
          "format", "Format",
          choiceNames = lapply(formats, function(f) {
            shiny::span(class = "hm-format", shiny::strong(f[2]),
                        shiny::span(f[3]))
          }),
          choiceValues = vapply(formats, function(f) f[1], ""),
          selected = image$format, inline = TRUE
        )
      ),
      shiny::div(
        class = "hm-seg",
        shiny::radioButtons("res", "Resolution",
                            choices = c("150 dpi" = "150", "300 dpi" = "300",
                                        "600 dpi" = "600"),
                            selected = as.character(image$res), inline = TRUE)
      ),
      shiny::textInput("filename", "File name", value = image$stem,
                       width = "320px")
    )
  )
}

#' The server of the app
#'
#' @param controls the settings of heat_map() (from .app_controls()).
#' @param data the data to start with (from .app_data()), or NULL for the
#'   start page.
#' @param start_code the starting settings as R code.
#' @param allow_code whether settings may be R code.
#' @noRd
.app_server <- function(controls,
                        data,
                        start_code = character(0),
                        allow_code = TRUE) {
  fun_controls <- list(
    heat_map_dot = .app_controls(
      "heat_map_dot", "dot",
      skip = c("x", "...", "features", "group_by", "assay")
    ),
    heat_map_cor = .app_controls("heat_map_cor", "cor", skip = c("x", "..."))
  )
  all_controls <- function(fun) {
    c(controls, fun_controls[[fun]])
  }
  function(input, output, session) {
    current <- shiny::reactiveVal(data)
    token <- shiny::reactiveVal(1)
    rows_keep <- shiny::reactiveVal(NULL)
    groups_keep <- shiny::reactiveVal(NULL)
    last_good <- shiny::reactiveVal(NULL)
    history <- shiny::reactiveValues(stack = NULL, pos = 0)
    previous <- NULL
    fun <- shiny::reactive(.app_fun(input$fun))

    # ---- data ----
    load <- function(d, example = NULL) {
      old <- shiny::isolate(current())
      if(!is.null(old)) {
        # (SETTINGS OF THE OLD DATA'S ROWS, COLUMNS AND CLUSTERS GO)
        codes <- shiny::isolate(settings_code())
        gone <- intersect(names(codes), .APP_DATA_SPECIFIC)
        for(nm in gone) .app_set_input(session, controls[[nm]], "")
        if(length(gone) > 0) {
          shiny::showNotification(
            paste0("Reset for the new data: ",
                   paste(vapply(gone, function(nm) controls[[nm]]$label, ""),
                         collapse = ", "), "."),
            duration = 8
          )
        }
      }
      if(!is.null(example)) {
        if(!is.null(example$group)) d$group <- example$group
        shiny::updateRadioButtons(session, "fun", selected = example$fun)
        for(nm in names(example$settings)) {
          .app_set_input(session, controls[[nm]], example$settings[[nm]])
        }
      }
      rows_keep(NULL)
      groups_keep(NULL)
      token(shiny::isolate(token()) + 1)
      current(d)
      session$sendCustomMessage("hm-view", list(view = "work"))
    }
    read_upload <- function(f) {
      d <- tryCatch(.app_read(f$datapath, f$name), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error",
                                duration = 12)
        NULL
      })
      if(!is.null(d)) load(d)
    }
    shiny::observeEvent(input$file, read_upload(input$file))
    shiny::observeEvent(input$file2, read_upload(input$file2))
    pick_example <- function(id) {
      card <- Filter(function(e) e$id == id, .APP_EXAMPLE_CARDS)[[1]]
      d <- tryCatch(.app_example(id), error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error")
        NULL
      })
      if(id == "cytometry") card$group <- "cluster"
      if(!is.null(d)) load(d, card)
    }
    lapply(.APP_EXAMPLE_CARDS, function(ex) {
      shiny::observeEvent(input[[paste0("example_", ex$id)]],
                          pick_example(ex$id))
    })
    shiny::observeEvent(input$example, {
      if(nzchar(input$example)) pick_example(input$example)
    }, ignoreInit = TRUE)

    output$data_chip_text <- shiny::renderUI({
      d <- current()
      if(is.null(d)) return(NULL)
      shiny::tagList(
        shiny::strong(d$name),
        shiny::span(class = "hm-muted",
                    shiny::HTML(paste0(
                      format(d$n, big.mark = ","),
                      if(d$kind == "cells") " cells &times; " else " rows &times; ",
                      format(length(d$features), big.mark = ","),
                      if(d$kind == "cells") " features" else " columns"))),
        shiny::span(class = "hm-chip-link", "Change data")
      )
    })
    output$data_card <- shiny::renderUI({
      d <- current()
      if(is.null(d)) return(NULL)
      named <- d$kind == "table" && !is.null(rownames(d$x)) &&
        !identical(rownames(d$x), as.character(seq_len(nrow(d$x))))
      shiny::div(
        class = "hm-data-card",
        shiny::strong(d$name),
        shiny::span(paste0(format(d$n, big.mark = ","),
                           if(d$kind == "cells") " cells, " else " rows, ",
                           format(length(d$features), big.mark = ","),
                           if(d$kind == "cells") " features" else " columns")),
        if(d$kind == "table") {
          shiny::span(class = "hm-muted",
                      if(named) "Rows named by the first column." else
                        "Rows numbered: no column holds a unique name for each row.")
        }
      )
    })
    output$has_assays <- shiny::reactive({
      d <- current()
      !is.null(d) && length(d$assays) > 0
    })
    shiny::outputOptions(output, "has_assays", suspendWhenHidden = FALSE)
    shiny::observeEvent(current(), {
      d <- current()
      shiny::updateSelectInput(session, "assay",
                               choices = c("Default" = "", d$assays))
    })

    # (THE INPUTS OF EACH DATA SET HAVE THEIR OWN IDS, SO VALUES LEFT BY THE
    # LAST DATA SET ARE NEVER READ)
    rid <- function(what, i = NULL) {
      paste0(what, "_", shiny::isolate(token()), if(!is.null(i)) paste0("_", i))
    }
    output$data_roles <- shiny::renderUI({
      d <- current()
      tok <- token()
      if(is.null(d)) return(NULL)
      id <- function(what, i = NULL) paste0(what, "_", tok, if(!is.null(i)) paste0("_", i))
      if(.app_roles_mode(d)) {
        roles <- .app_default_roles(d)
        x <- d$x
        return(shiny::div(
          class = "hm-roles",
          shiny::h3(class = "hm-h3", "What each column does"),
          lapply(seq_along(d$features), function(i) {
            v <- if(is.data.frame(x)) x[[i]] else x[, i]
            shiny::div(
              class = "hm-role",
              shiny::div(class = "hm-role-name",
                         shiny::strong(d$features[i]),
                         shiny::span(.app_column_text(v))),
              shiny::selectInput(id("role", i), NULL,
                                 choices = .app_role_choices(d, i),
                                 selected = roles[i], selectize = FALSE,
                                 width = "100%")
            )
          })
        ))
      }
      shiny::tagList(
        shiny::selectizeInput(
          id("features"), shiny::tagList(
            shiny::span(class = "hm-lab",
                        if(d$kind == "cells") "Features to show" else
                          "Columns to show"),
            shiny::tags$code(class = "hm-arg", "features")),
          choices = d$shown, selected = d$shown, multiple = TRUE,
          width = "100%",
          options = list(placeholder = "All of them",
                         plugins = list("remove_button"))
        ),
        shiny::selectizeInput(
          id("group"), shiny::tagList(
            shiny::span(class = "hm-lab", "Group the rows by"),
            shiny::tags$code(class = "hm-arg", "group_by")),
          choices = d$meta, selected = d$group, multiple = TRUE,
          width = "100%", options = list(placeholder = "Nothing")
        ),
        shiny::selectizeInput(
          id("annotate"), shiny::tagList(
            shiny::span(class = "hm-lab", "Annotate the rows by"),
            shiny::tags$code(class = "hm-arg", "annotation_by")),
          choices = utils::head(d$annotate, 50), selected = character(0),
          multiple = TRUE,
          width = "100%", options = list(placeholder = "Nothing")
        )
      )
    })
    # (ON THE PAGE WHILE THE DATA SECTION IS CLOSED, SO THAT SELECTIONS ON
    # THE HEATMAP CAN CHANGE THEM)
    shiny::outputOptions(output, "data_roles", suspendWhenHidden = FALSE)
    # (EVERY FEATURE CAN BE FOUND BY TYPING, FROM THE SERVER)
    shiny::observeEvent(token(), {
      d <- current()
      if(is.null(d) || .app_roles_mode(d)) return()
      shiny::updateSelectizeInput(session, rid("features"),
                                  choices = d$features, selected = d$shown,
                                  server = TRUE)
      shiny::updateSelectizeInput(session, rid("annotate"),
                                  choices = d$annotate, selected = character(0),
                                  server = TRUE)
    })
    data_code <- shiny::reactive({
      d <- current()
      if(is.null(d)) return(character(0))
      tok <- token()
      id <- function(what, i = NULL) paste0(what, "_", tok, if(!is.null(i)) paste0("_", i))
      if(.app_roles_mode(d)) {
        defaults <- .app_default_roles(d)
        roles <- vapply(seq_along(d$features), function(i) {
          r <- input[[id("role", i)]]
          if(is.null(r)) defaults[i] else r
        }, character(1))
        show <- d$features[roles == "show"]
        features <- if(all(roles == "show")) NULL else show
        group <- d$features[roles == "group"]
        annotate <- d$features[roles == "annotate"]
        return(.app_data_code(d, features, group, annotate, input$assay))
      }
      # (AN EMPTY PICKER IS NULL, AS ONE NOT ON THE PAGE YET: ONLY THOSE NOT
      # SEEN YET TAKE THEIR FIRST VALUES)
      seen <- picked()
      feats <- input[[id("features")]]
      group <- input[[id("group")]]
      annotate <- input[[id("annotate")]]
      if(is.null(feats) && !id("features") %in% seen) feats <- d$shown
      if(is.null(group) && !id("group") %in% seen) group <- d$group
      .app_data_code(d, feats, group, annotate, input$assay)
    })
    picked <- shiny::reactiveVal(character(0))
    shiny::observe({
      tok <- token()
      ids <- paste0(c("features", "group", "annotate"), "_", tok)
      have <- ids[!vapply(ids, function(i) is.null(input[[i]]), logical(1))]
      old <- shiny::isolate(picked())
      if(length(setdiff(have, old)) > 0) picked(union(old, have))
    })
    # (THE ROWS NO LONGER GROUPED, E.G. TO SPLIT THEM INTO CLUSTERS: THE
    # GROUPS' COLUMNS ANNOTATE THEM INSTEAD)
    ungroup <- function() {
      d <- shiny::isolate(current())
      if(is.null(d)) return()
      if(.app_roles_mode(d)) {
        roles <- .app_default_roles(d)
        for(i in seq_along(d$features)) {
          r <- shiny::isolate(input[[rid("role", i)]])
          if(is.null(r)) r <- roles[i]
          if(r == "group") {
            shiny::updateSelectInput(session, rid("role", i),
                                     selected = "annotate")
          }
        }
      } else {
        groups <- .app_code_value(.app_arg(shiny::isolate(data_code()),
                                           "group_by"))
        annotate <- shiny::isolate(input[[rid("annotate")]])
        shiny::updateSelectizeInput(session, rid("group"),
                                    selected = character(0))
        shiny::updateSelectizeInput(session, rid("annotate"),
                                    choices = d$annotate,
                                    selected = union(annotate,
                                                     intersect(groups, d$annotate)),
                                    server = TRUE)
      }
    }

    # ---- settings ----
    output$fun_title <- shiny::renderText(
      switch(fun(), heat_map_dot = "Dot plot", heat_map_cor = "Correlation", "")
    )
    output$fun_rail <- shiny::renderText(
      switch(fun(), heat_map_dot = "Dot plot", heat_map_cor = "Correlation", "")
    )
    shiny::outputOptions(output, "fun_rail", suspendWhenHidden = FALSE)
    # (THE SETTINGS OF DOT PLOTS AND CORRELATIONS, ONLY THOSE OF THE PLOT
    # SHOWN: BOTH STAY ON THE PAGE, SO THEIR VALUES ARE KEPT WHEN THE PLOT
    # CHANGES, AND THROUGH A CHANGE OF DATA)
    output$fun_settings <- shiny::renderUI({
      two <- .app_two_values(current())
      hints <- c(
        heat_map_dot = "One dot for each group (Data: group the rows) and feature: its colour is the mean, its size the share of cells detected.",
        heat_map_cor = "The correlations between the columns shown."
      )
      shown <- shiny::isolate(fun())
      shiny::tagList(lapply(names(fun_controls), function(f) {
        ctl <- lapply(fun_controls[[f]], function(k) {
          if(k$type == "meta") {
            k$choices <- c("Nothing" = "", two)
            if(length(two) == 0) {
              k$help <- "No column of the data has two values to split by."
            }
          }
          k
        })
        now <- shiny::isolate(.app_settings_code(ctl, input))
        shiny::div(class = "hm-fun-block", `data-fun` = f,
                   hidden = if(f != shown) NA,
                   shiny::p(class = "hm-hint", hints[[f]]),
                   .app_controls_ui(ctl, now))
      }))
    })
    # (ON THE PAGE WHILE ITS SECTION IS CLOSED, FOR UNDO AND REDO)
    shiny::outputOptions(output, "fun_settings", suspendWhenHidden = FALSE)
    # (EVERY SETTING SET: OF heat_map() AND OF EACH PLOT, WITH THE PLOT SHOWN)
    state <- shiny::reactive({
      c(list(fun = fun(), heat_map = .app_settings_code(controls, input)),
        lapply(fun_controls, function(fc) .app_settings_code(fc, input)))
    })
    # (THE SETTINGS OF THE PLOT SHOWN: ITS OWN ARGUMENTS REPLACE THOSE OF
    # heat_map(), AND THOSE IT DOESN'T USE ARE LEFT OUT - KEPT FOR THE NEXT
    # HEATMAP)
    settings_code <- shiny::reactive({
      st <- state()
      out <- st$heat_map
      fc <- fun_controls[[st$fun]]
      if(!is.null(fc)) {
        out <- out[setdiff(names(out), c(names(fc), .APP_NOT_FOR[[st$fun]]))]
        out <- c(out, st[[st$fun]])
      }
      out
    })
    # (SETTINGS OF heat_map() THE PLOT DOESN'T USE ARE HIDDEN)
    shiny::observe({
      fc <- fun_controls[[fun()]]
      session$sendCustomMessage("hm-unused", list(
        names = as.list(intersect(c(names(fc), .APP_NOT_FOR[[fun()]]),
                                  names(controls))),
        fun = fun()
      ))
    })
    # (THE KIND OF PLOT WAITS WITH ITS SETTINGS, SO ONE IS NEVER DRAWN WITH
    # THE SETTINGS OF ANOTHER)
    plot_d <- shiny::debounce(shiny::reactive(list(fun = fun(),
                                                   settings = settings_code(),
                                                   state = state())),
                              350)
    settings_d <- shiny::reactive(plot_d()$settings)

    # ---- clusters or groups of the rows: the latest choice wins ----
    # (heat_map() GROUPS CELLS BY group_by OR BY CLUSTERS, NOT BOTH: SPLITTING
    # GROUPED ROWS INTO CLUSTERS STOPS GROUPING THEM - THE GROUPS ANNOTATE
    # THEM INSTEAD - AND GROUPING THE ROWS STOPS SPLITTING THEM. DOT PLOTS
    # CLUSTER THEIR GROUPS, SO KEEP BOTH)
    and <- function(x) {
      if(length(x) < 2) x else paste(paste(x[-length(x)], collapse = ", "),
                                     "and", x[length(x)])
    }
    last_cuts <- character(0)
    shiny::observe({
      s <- settings_code()
      cuts <- s[.app_cell_cuts(s)]
      new <- length(cuts) > 0 && !identical(cuts, last_cuts)
      last_cuts <<- cuts
      if(!new || shiny::isolate(fun()) != "heat_map") return()
      groups <- .app_code_value(.app_arg(shiny::isolate(data_code()),
                                         "group_by"))
      if(length(groups) == 0) return()
      ungroup()
      shiny::showNotification(
        paste0("The rows are split into clusters instead of grouped by ",
               and(groups), ", which ", if(length(groups) == 1) "annotates" else
                 "annotate", " them now."),
        duration = 8
      )
    })
    last_group <- NULL
    last_token <- NULL
    shiny::observe({
      g <- .app_arg(data_code(), "group_by")
      tok <- token()
      # (A NEW DATA SET STARTS WITH ITS OWN GROUPS, AND ITS CLUSTERS RESET)
      if(!identical(tok, last_token)) {
        last_token <<- tok
        last_group <<- g
        return()
      }
      changed <- !identical(g, last_group)
      last_group <<- g
      if(!changed || is.null(g) || shiny::isolate(fun()) != "heat_map") return()
      s <- shiny::isolate(settings_code())
      cuts <- .app_cell_cuts(s)
      if(length(cuts) == 0) return()
      for(nm in cuts) .app_set_input(session, controls[[nm]], "")
      shiny::showNotification(
        paste0("The rows are grouped by ", and(.app_code_value(g)),
               ", so they're no longer split into clusters."),
        duration = 8
      )
    })

    # (THE NUMBER OF CHANGED SETTINGS IN EACH SECTION, ON THE RAIL)
    shiny::observe({
      s <- names(settings_code())
      ctls <- all_controls(fun())
      sec <- vapply(s, function(nm) ctls[[nm]]$section, character(1))
      session$sendCustomMessage("hm-badges", as.list(table(sec)))
    })
    output$changed <- shiny::renderUI({
      s <- settings_code()
      ctls <- all_controls(fun())
      keep <- rows_keep()
      groups <- groups_keep()
      if(length(s) == 0 && is.null(keep) && is.null(groups)) {
        return(shiny::div(class = "hm-chips hm-chips-empty",
                          shiny::span(class = "hm-muted",
                                      "Settings you change show here, each with a button to undo it.")))
      }
      chip <- function(name, label, value) {
        shiny::span(class = "hm-chip", paste0(label, ": ", value),
                    shiny::tags$button(type = "button", class = "hm-chip-x",
                                       `data-reset` = name,
                                       `aria-label` = paste("Reset", label),
                                       shiny::HTML("&times;")))
      }
      shiny::div(
        class = "hm-chips", role = "list", `aria-label` = "Changed settings",
        shiny::span(class = "hm-muted", "Changed"),
        if(!is.null(keep)) chip("__rows", "Rows", paste(length(keep), "shown")),
        if(!is.null(groups)) {
          chip("__groups", "Groups", paste(length(groups$values), "shown"))
        },
        lapply(names(s), function(nm) {
          chip(nm, ctls[[nm]]$label, .app_chip_value(ctls[[nm]], s[[nm]]))
        }),
        shiny::tags$button(type = "button", class = "hm-link",
                           `data-reset` = "__all", "Reset all")
      )
    })
    reset <- function(names) {
      ctls <- all_controls(fun())
      for(nm in names) {
        if(nm == "__rows") {
          rows_keep(NULL)
        } else if(nm == "__groups") {
          groups_keep(NULL)
        } else if(nm == "__group") {
          ungroup()
        } else if(!is.null(ctls[[nm]])) {
          .app_set_input(session, ctls[[nm]], "")
        }
      }
    }
    shiny::observeEvent(input$reset_one, {
      nm <- input$reset_one
      if(identical(nm, "__all")) {
        reset(c("__rows", "__groups", names(settings_code())))
      } else {
        reset(nm)
      }
    })
    shiny::observeEvent(input$reset_section, {
      sec <- input$reset_section
      ctls <- all_controls(fun())
      reset(Filter(function(nm) identical(ctls[[nm]]$section, sec),
                   names(settings_code())))
    })

    # ---- undo and redo ----
    # (EACH STEP HOLDS THE PLOT AND ALL ITS SETTINGS, SO THAT A CHANGE OF PLOT
    # IS UNDONE TOO)
    shiny::observeEvent(plot_d(), {
      p <- plot_d()
      if(is.null(history$stack)) {
        history$stack <- list(p)
        history$pos <- 1
      } else if(!identical(p$state, history$stack[[history$pos]]$state)) {
        stack <- history$stack[seq_len(history$pos)]
        stack[[length(stack) + 1]] <- p
        history$stack <- utils::tail(stack, 200)
        history$pos <- length(history$stack)
      }
      session$sendCustomMessage("hm-history", list(
        undo = history$pos > 1, redo = history$pos < length(history$stack)
      ))
    }, ignoreNULL = FALSE)
    go <- function(to) {
      target <- history$stack[[to]]$state
      now <- shiny::isolate(state())
      if(!identical(now$fun, target$fun)) {
        shiny::updateRadioButtons(session, "fun", selected = target$fun)
      }
      for(g in setdiff(names(target), "fun")) {
        ctls <- if(g == "heat_map") controls else fun_controls[[g]]
        a <- now[[g]]
        b <- target[[g]]
        for(nm in union(names(a), names(b))) {
          if(!is.null(ctls[[nm]]) && !identical(a[nm], b[nm])) {
            .app_set_input(session, ctls[[nm]],
                           if(nm %in% names(b)) b[[nm]] else "")
          }
        }
      }
      history$pos <- to
      session$sendCustomMessage("hm-history", list(
        undo = to > 1, redo = to < length(history$stack)
      ))
    }
    shiny::observeEvent(input$undo, {
      if(history$pos > 1) go(history$pos - 1)
    })
    shiny::observeEvent(input$redo, {
      if(history$pos < length(history$stack)) go(history$pos + 1)
    })
    last_change <- shiny::reactive({
      if(is.null(history$stack) || history$pos < 2) return("")
      now <- history$stack[[history$pos]]$settings
      before <- history$stack[[history$pos - 1]]$settings
      diff <- names(now)[!names(now) %in% names(before) |
                           now != before[names(now)]]
      if(length(diff) == 0) "" else diff[length(diff)]
    })

    # ---- the heatmap ----
    built <- shiny::reactive({
      d <- current()
      shiny::req(d)
      p <- plot_d()
      out <- .app_build(d, p$fun, c(data_code(), p$settings), rows_keep(),
                        allow_code, previous, groups_keep())
      if(!is.null(out$hm)) previous <<- out$hm
      out
    })
    shiny::observe({
      b <- built()
      if(!is.null(b$hm)) last_good(b)
    })
    size <- shiny::reactive({
      s <- input$size
      if(is.null(s) || s == "custom") {
        w <- input$width
        h <- input$height
        ok <- function(v) is.numeric(v) && length(v) == 1 && is.finite(v) && v > 0
        return(list(width = if(ok(w)) min(w, 60) else 7,
                    height = if(ok(h)) min(h, 60) else 7))
      }
      wh <- as.numeric(strsplit(s, "x", fixed = TRUE)[[1]])
      list(width = wh[1], height = wh[2])
    })
    # (THE IMAGE OF THE HEATMAP: ITS TIME ADDS TO THE BUILD'S, AND AN ERROR
    # DRAWING IT IS SHOWN AS THE BUILD'S ERRORS ARE)
    drawn_time <- shiny::reactiveVal(NULL)
    draw_error <- shiny::reactiveVal(NULL)
    shiny::observe({
      b <- last_good()
      shiny::req(b$hm)
      s <- size()
      t0 <- proc.time()[["elapsed"]]
      p <- tryCatch(.app_preview(b$hm, s$width, s$height),
                    error = function(e) e)
      if(inherits(p, "error")) {
        draw_error(conditionMessage(p))
        return()
      }
      draw_error(NULL)
      drawn_time(b$time + proc.time()[["elapsed"]] - t0)
      session$sendCustomMessage("hm-draw", p)
    })
    output$issue <- shiny::renderUI({
      d <- current()
      if(is.null(d)) return(NULL)
      b <- built()
      if(is.null(b$error) && !is.null(draw_error())) {
        b$error <- draw_error()
      }
      if(is.null(b$error)) return(NULL)
      s <- settings_d()
      ctls <- all_controls(fun())
      named <- names(s)[vapply(names(s), function(nm) {
        grepl(paste0("(^|[^A-Za-z0-9_.])", gsub(".", "\\.", nm, fixed = TRUE),
                     "([^A-Za-z0-9_.]|$)"), b$error)
      }, logical(1))]
      # (GROUPS ARE CHOSEN WITH THE DATA, NOT IN THE SETTINGS)
      grouped <- "group_by" %in% names(data_code()) &&
        grepl("'group_by'", b$error, fixed = TRUE)
      shiny::div(
        class = "hm-issue", role = "alert",
        .app_icon("warn", 20),
        shiny::div(
          shiny::strong("The heatmap couldn't be drawn."),
          shiny::span(b$error),
          shiny::span(class = "hm-muted",
                      if(!is.null(last_good())) "Showing the last heatmap that worked."),
          if(length(named) > 0 || grouped) {
            shiny::div(
              class = "hm-issue-actions",
              lapply(named, function(nm) {
                shiny::tags$button(type = "button", class = "hm-btn hm-small",
                                   `data-reset` = nm,
                                   paste("Reset", tolower(ctls[[nm]]$label)))
              }),
              if(grouped) {
                shiny::tags$button(type = "button", class = "hm-btn hm-small",
                                   `data-reset` = "__group",
                                   "Stop grouping the rows")
              }
            )
          }
        )
      )
    })
    output$status <- shiny::renderUI({
      b <- last_good()
      if(is.null(b)) return(NULL)
      check <- .app_colour_text(b$hm)
      notes <- b$notes
      time <- drawn_time()
      if(is.null(time)) time <- b$time
      shiny::div(
        class = "hm-status",
        shiny::span(class = "hm-ok",
                    if(time < 0.05) "Drawn in under 0.1 s" else
                      paste0("Drawn in ", format(round(time, 1), nsmall = 1),
                             " s")),
        if(nzchar(check)) shiny::span(check),
        if(length(notes) > 0) {
          shiny::tags$details(
            shiny::tags$summary(paste0("Messages (", length(notes), ")")),
            lapply(notes, shiny::p)
          )
        }
      )
    })
    # (THE HISTOGRAM UNDER THE LIMITS OF THE COLOUR SCALE)
    lim <- controls$cell_col_scale_limits
    output[[paste0(lim$id, "_hist")]] <- shiny::renderUI({
      b <- last_good()
      if(is.null(b)) return(NULL)
      values <- suppressWarnings(as.numeric(as.matrix(heat_map_info(b$hm)$data)))
      code <- .app_ctl_code(lim, input)
      limits <- if(identical(code, '"robust"')) {
        stats::quantile(values, c(0.02, 0.98), na.rm = TRUE, names = FALSE)
      } else if(nzchar(code)) {
        eval(parse(text = code)[[1]], baseenv())
      }
      .app_histogram(values, limits)
    })
    # (ROWS AND COLUMNS TO MARK AND ZOOM, FROM THE HEATMAP)
    names_seen <- shiny::reactiveVal(list())
    shiny::observe({
      b <- last_good()
      shiny::req(b$hm)
      info <- heat_map_info(b$hm)
      rn <- rownames(info$data)
      cn <- colnames(info$data)
      seen <- shiny::isolate(names_seen())
      if(!identical(seen$rows, rn)) {
        for(nm in c("mark_y", "zoom_y")) {
          shiny::updateSelectizeInput(
            session, controls[[nm]]$id, choices = rn,
            selected = shiny::isolate(input[[controls[[nm]]$id]]), server = TRUE
          )
        }
      }
      if(!identical(seen$cols, cn)) {
        for(nm in c("mark_x", "zoom_x")) {
          shiny::updateSelectizeInput(
            session, controls[[nm]]$id, choices = cn,
            selected = shiny::isolate(input[[controls[[nm]]$id]]), server = TRUE
          )
        }
      }
      names_seen(list(rows = rn, cols = cn))
    })

    # ---- what is selected on the heatmap ----
    shiny::observeEvent(input$sel_show, {
      sel <- input$hm_selection
      d <- current()
      if(is.null(d) || is.null(sel)) return()
      # (WHAT THE LABELS ARE - FEATURES, ROWS OF A TABLE OR GROUPS - WHICHEVER
      # SIDE THEY ARE ON, E.G. WITH THE AXES SWAPPED)
      keep <- .app_selection(d, c(unlist(sel$rows), unlist(sel$cols)),
                             fun(), data_code())
      if(length(keep$features) > 0) {
        if(.app_roles_mode(d)) {
          for(i in seq_along(d$features)) {
            r <- input[[rid("role", i)]]
            if(is.null(r)) r <- .app_default_roles(d)[i]
            if(r == "show" && !d$features[i] %in% keep$features) {
              shiny::updateSelectInput(session, rid("role", i),
                                       selected = "leave")
            }
          }
        } else {
          shiny::updateSelectizeInput(session, rid("features"),
                                      choices = d$features,
                                      selected = keep$features, server = TRUE)
        }
      }
      if(length(keep$rows) > 0) {
        rows_keep(keep$rows)
      }
      if(length(keep$groups) > 0) {
        groups_keep(keep$groups)
      }
      if(length(keep$features) + length(keep$rows) + length(keep$groups) == 0) {
        shiny::showNotification(
          paste("These rows can't be kept here: only the rows of tables,",
                "the groups of dot plots and the features are."),
          duration = 8
        )
      }
      session$sendCustomMessage("hm-sel-clear", list())
    })
    shiny::observeEvent(input$sel_label, {
      sel <- input$hm_selection
      if(is.null(sel)) return()
      rows <- unlist(sel$rows)
      cols <- unlist(sel$cols)
      if(length(rows) > 0) .app_set_input(session, controls$mark_y,
                                          .app_deparse(rows))
      if(length(cols) > 0) .app_set_input(session, controls$mark_x,
                                          .app_deparse(cols))
      session$sendCustomMessage("hm-sel-clear", list())
    })

    # ---- code and downloads ----
    image <- shiny::reactive({
      s <- size()
      fmt <- input$format
      if(length(fmt) != 1 || !fmt %in% c("png", "pdf", "svg", "tiff", "jpeg")) {
        fmt <- "png"
      }
      res <- suppressWarnings(as.numeric(input$res))
      if(length(res) != 1 || !is.finite(res) || res <= 0) {
        res <- 300
      }
      d <- current()
      stem <- input$filename
      if(is.null(stem) || !nzchar(trimws(stem))) {
        stem <- paste0(if(is.null(d)) "heatmap" else d$name, "-heatmap")
      }
      stem <- gsub("[^A-Za-z0-9._-]+", "-", trimws(stem))
      list(file = paste0(stem, ".", fmt), stem = stem, width = s$width,
           height = s$height, res = min(res, 2400), format = fmt)
    })
    code <- shiny::reactive({
      d <- current()
      if(is.null(d)) return("")
      .app_code(d, fun(), c(data_code(), settings_code()), image(),
                rows = rows_keep(), groups = groups_keep())
    })
    shiny::observe({
      session$sendCustomMessage("hm-code", list(code = code(),
                                                changed = last_change()))
    })
    code_file <- function() paste0(image()$stem, ".R")
    output$code_download <- shiny::downloadHandler(
      filename = code_file, content = function(file) writeLines(code(), file)
    )
    output$code_download_modal <- shiny::downloadHandler(
      filename = code_file, content = function(file) writeLines(code(), file)
    )
    shiny::observeEvent(input$download_open, {
      shiny::showModal(.app_download_modal(image(), current()$name))
    })
    output$download <- shiny::downloadHandler(
      filename = function() image()$file,
      content = function(file) {
        b <- last_good()
        if(is.null(b)) stop("There is no heatmap to save yet.", call. = FALSE)
        .app_save(b$hm, file, image())
      }
    )
  }
}
