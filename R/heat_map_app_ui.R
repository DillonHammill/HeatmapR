## THE PAGE OF THE APP ---------------------------------------------------------

#' The examples on the start page: id, title, text, picture and how the app
#' sets them up (the kind of heatmap, the groups and settings)
#' @noRd
.APP_EXAMPLE_CARDS <- list(
  list(id = "mtcars", title = "Cars",
       text = "mtcars: 32 cars, 11 measurements. A clustered heatmap.",
       img = "ex-mtcars.png", fun = "heat_map",
       settings = c(scale = '"column"', tree = '"both"')),
  list(id = "iris", title = "Flowers",
       text = "iris: 150 flowers of 3 species, annotated by species.",
       img = "ex-iris.png", fun = "heat_map",
       settings = c(scale = '"column"', tree = '"y"')),
  list(id = "rna", title = "Single cells: RNA",
       text = "3,000 blood cells of 6 types. A dot plot of marker genes.",
       img = "ex-rna.png", fun = "heat_map_dot", settings = character(0)),
  list(id = "cytometry", title = "Single cells: cytometry",
       text = "20,000 events, 12 markers. Cells grouped by cluster.",
       img = "ex-cyto.png", fun = "heat_map",
       settings = c(transform = '"asinh"', scale = '"column"',
                    group_max = "100"))
)

#' An icon of the app (stroked SVG, hidden from screen readers)
#' @noRd
.app_icon <- function(name,
                      size = 20) {
  paths <- switch(
    name,
    data = '<rect x="3" y="4" width="18" height="16" rx="2"></rect><path d="M3 10h18M9 4v16"></path>',
    fun = '<circle cx="7" cy="7" r="3"></circle><circle cx="17" cy="7" r="2"></circle><circle cx="7" cy="17" r="2"></circle><circle cx="17" cy="17" r="3.5"></circle>',
    scale = '<path d="M4 20v-6h6V8h6V4M16 4v16M10 8v12M4 14v6"></path>',
    colours = '<rect x="3" y="3" width="8" height="8" rx="1"></rect><rect x="13" y="3" width="8" height="8" rx="1"></rect><rect x="3" y="13" width="8" height="8" rx="1"></rect><rect x="13" y="13" width="8" height="8" rx="1"></rect>',
    cells = '<rect x="4" y="4" width="16" height="16" rx="2"></rect><path d="M4 12h16M12 4v16"></path>',
    labels = '<path d="M5 6h14M12 6v13M9 19h6"></path>',
    legend = '<rect x="4" y="3" width="5" height="18" rx="1"></rect><path d="M13 6h7M13 12h7M13 18h7"></path>',
    tracks = '<path d="M4 20V10M10 20V4M16 20v-7M22 20H2"></path>',
    marks = '<circle cx="11" cy="11" r="6"></circle><path d="M20 20l-4.5-4.5M11 8v6M8 11h6"></path>',
    all = '<path d="M4 6h10M18 6h2M4 12h4M12 12h8M4 18h12"></path><circle cx="16" cy="6" r="2"></circle><circle cx="10" cy="12" r="2"></circle><circle cx="18" cy="18" r="2"></circle>',
    undo = '<path d="M9 14L4 9l5-5"></path><path d="M4 9h10.5a5.5 5.5 0 010 11H11"></path>',
    redo = '<path d="M15 14l5-5-5-5"></path><path d="M20 9H9.5a5.5 5.5 0 000 11H13"></path>',
    code = '<path d="M8 6l-6 6 6 6M16 6l6 6-6 6"></path>',
    download = '<path d="M12 4v11M7 10l5 5 5-5M5 20h14"></path>',
    upload = '<path d="M12 15V4M7 9l5-5 5 5"></path><path d="M4 15v3a2 2 0 002 2h12a2 2 0 002-2v-3"></path>',
    info = '<circle cx="12" cy="12" r="9"></circle><path d="M12 11v5M12 8h.01"></path>',
    search = '<circle cx="11" cy="11" r="7"></circle><path d="M20 20l-4-4"></path>',
    close = '<path d="M6 6l12 12M18 6L6 18"></path>',
    check = '<path d="M5 12l5 5 9-10"></path>',
    warn = '<path d="M12 3l10 18H2z"></path><path d="M12 10v5M12 18h.01"></path>',
    ""
  )
  shiny::HTML(paste0(
    '<svg class="hm-icon" width="', size, '" height="', size,
    '" viewBox="0 0 24 24" fill="none" stroke="currentColor" ',
    'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" ',
    'aria-hidden="true" focusable="false">', paths, '</svg>'
  ))
}

#' The strip of the nine colours of HeatmapR's colour scale (as in the logo)
#' @noRd
.app_strip <- function(cols = NULL,
                       class = "hm-strip") {
  if(is.null(cols)) {
    cols <- grDevices::colorRampPalette(.default_col_scale())(9)
  }
  shiny::div(
    class = class, `aria-hidden` = "true",
    lapply(substr(cols, 1, 7), function(col) {
      shiny::span(style = paste0("background:", col))
    })
  )
}

#' The nine colours of a palette's swatch
#' @noRd
.app_swatch <- function(code) {
  if(!nzchar(code)) {
    return(grDevices::colorRampPalette(.default_col_scale())(9))
  }
  name <- eval(parse(text = code)[[1]])
  grDevices::hcl.colors(9, name)
}

#' The label of a setting: its plain name and the name of its argument
#' @noRd
.app_label_tag <- function(ctl) {
  shiny::tagList(
    shiny::span(class = "hm-lab", ctl$label),
    shiny::tags$code(class = "hm-arg", ctl$name)
  )
}

#' The input(s) of a setting
#'
#' @param ctl the setting (from .app_controls()).
#' @param code its starting value as R code ("" for the default).
#' @noRd
.app_control_ui <- function(ctl,
                            code = "") {
  vals <- .app_input_values(ctl, code)
  id <- ctl$id
  v <- vals[[id]]
  label <- .app_label_tag(ctl)
  choices <- ctl$choices
  if(ctl$type %in% c("choice", "seg") && !is.null(v) && !v %in% choices) {
    choices <- c(choices, stats::setNames(v, v))
  }
  input <- switch(
    ctl$type,
    choice = ,
    meta = shiny::selectInput(id, label, choices = choices, selected = v,
                              width = "100%", selectize = FALSE),
    seg = shiny::div(
      class = "hm-seg",
      shiny::radioButtons(id, label, choices = choices, selected = v,
                          inline = TRUE, width = "100%")
    ),
    logical = shiny::div(
      class = "hm-check",
      shiny::checkboxInput(id, label, value = isTRUE(v), width = "100%")
    ),
    number = shiny::numericInput(id, label,
                                 value = if(is.numeric(v) && !is.na(v)) v else "",
                                 width = "100%"),
    count = shiny::div(
      class = "hm-count-input",
      shiny::tags$label(`for` = id, class = "control-label", label),
      shiny::div(
        class = "hm-stepper",
        shiny::tags$button(type = "button", class = "hm-step",
                           `data-for` = id, `data-delta` = "-1",
                           `aria-label` = paste("Fewer:", ctl$label), shiny::HTML("&minus;")),
        shiny::tags$input(id = id, type = "number", class = "form-control",
                          min = "0", step = "1", placeholder = "none",
                          value = if(is.numeric(v) && !is.na(v)) v else NULL),
        shiny::tags$button(type = "button", class = "hm-step",
                           `data-for` = id, `data-delta` = "1",
                           `aria-label` = paste("More:", ctl$label), "+")
      )
    ),
    text = shiny::textInput(id, label, value = v, width = "100%"),
    colour = shiny::div(
      class = "hm-colour",
      shiny::textInput(id, label, value = v, width = "100%"),
      shiny::tags$input(type = "color", class = "hm-swatch",
                        `aria-label` = paste("Pick", ctl$label),
                        `data-for` = id)
    ),
    tree = shiny::div(
      class = "hm-cards",
      shiny::checkboxGroupInput(id, label,
                                choices = c("Rows" = "y", "Columns" = "x"),
                                selected = v, inline = TRUE, width = "100%")
    ),
    palette = {
      opts <- c(.APP_PALETTES, "More palettes" = "__more",
                "Your own colours" = "__own")
      names_ui <- lapply(seq_along(opts), function(i) {
        swatch <- if(i <= length(.APP_PALETTES)) {
          .app_strip(.app_swatch(opts[[i]]), class = "hm-pal-strip")
        }
        shiny::span(class = "hm-pal", shiny::span(class = "hm-pal-name",
                                                   names(opts)[i]), swatch)
      })
      shiny::tagList(
        shiny::div(
          class = "hm-palettes",
          shiny::radioButtons(id, label, choiceNames = names_ui,
                              choiceValues = unname(opts), selected = v,
                              width = "100%")
        ),
        shiny::conditionalPanel(
          paste0("input['", id, "'] == '__more'"),
          shiny::selectizeInput(
            paste0(id, "_more"), NULL,
            choices = .app_q(grDevices::hcl.pals()),
            selected = vals[[paste0(id, "_more")]], width = "100%"
          )
        ),
        shiny::conditionalPanel(
          paste0("input['", id, "'] == '__own'"),
          shiny::textInput(paste0(id, "_own"), NULL,
                           value = if(is.null(vals[[paste0(id, "_own")]])) ""
                             else vals[[paste0(id, "_own")]],
                           placeholder = "e.g. navy, white, firebrick",
                           width = "100%")
        )
      )
    },
    centre = shiny::div(
      class = "hm-centre",
      shiny::div(class = "hm-check",
                 shiny::checkboxInput(paste0(id, "_on"), label,
                                      value = isTRUE(vals[[paste0(id, "_on")]]))),
      shiny::numericInput(id, NULL, value = v, width = "110px")
    ),
    limits = shiny::tagList(
      shiny::div(
        class = "hm-seg",
        shiny::radioButtons(paste0(id, "_mode"), label,
                            choices = c("Full range" = "full",
                                        "Robust" = "robust",
                                        "Custom" = "custom"),
                            selected = vals[[paste0(id, "_mode")]],
                            inline = TRUE, width = "100%")
      ),
      shiny::uiOutput(paste0(id, "_hist")),
      shiny::conditionalPanel(
        paste0("input['", id, "_mode'] == 'custom'"),
        shiny::div(
          class = "hm-pair hm-range",
          shiny::numericInput(paste0(id, "_from"), "From",
                              value = if(is.null(vals[[paste0(id, "_from")]]))
                                "" else vals[[paste0(id, "_from")]]),
          shiny::numericInput(paste0(id, "_to"), "to",
                              value = if(is.null(vals[[paste0(id, "_to")]]))
                                "" else vals[[paste0(id, "_to")]])
        )
      )
    ),
    rows = ,
    cols = shiny::selectizeInput(
      id, label, choices = v, selected = v, multiple = TRUE, width = "100%",
      options = list(placeholder = if(ctl$type == "rows") "Choose rows" else
        "Choose columns", plugins = list("remove_button"))
    ),
    shiny::div(
      class = "hm-code-input",
      shiny::textInput(id, label, value = v, width = "100%",
                       placeholder = if(ctl$default %in% c("", "NULL"))
                         "Not set" else ctl$default)
    )
  )
  help <- if(nzchar(ctl$help)) ctl$help else ""
  shiny::div(
    class = paste("hm-setting", paste0("hm-type-", ctl$type)),
    `data-name` = ctl$name,
    `data-search` = tolower(paste(ctl$label, ctl$name, ctl$doc)),
    `data-label` = tolower(paste(ctl$label, ctl$name)),
    if(nzchar(ctl$doc)) {
      shiny::tags$button(type = "button", class = "hm-info",
                         `aria-label` = paste("About", ctl$label),
                         `aria-expanded` = "false",
                         .app_icon("info", 16))
    },
    input,
    if(nzchar(help)) shiny::p(class = "hm-hint", help),
    if(ctl$type == "code" && !nzchar(help)) {
      shiny::p(class = "hm-hint", "R code, with your data as ",
               shiny::tags$code("x"), ".")
    },
    if(nzchar(ctl$doc)) shiny::div(class = "hm-doc", hidden = NA, ctl$doc)
  )
}

#' The settings of a section, pairs of short ones side by side
#' @noRd
.app_controls_ui <- function(controls,
                             start_code = character(0)) {
  out <- list()
  i <- 1
  code <- function(ctl) {
    if(ctl$name %in% names(start_code)) start_code[[ctl$name]] else ""
  }
  while(i <= length(controls)) {
    ctl <- controls[[i]]
    nxt <- if(i < length(controls)) controls[[i + 1]] else NULL
    if(ctl$half && !is.null(nxt) && nxt$half) {
      out[[length(out) + 1]] <- shiny::div(
        class = "hm-pair",
        .app_control_ui(ctl, code(ctl)),
        .app_control_ui(nxt, code(nxt))
      )
      i <- i + 2
    } else {
      out[[length(out) + 1]] <- .app_control_ui(ctl, code(ctl))
      i <- i + 1
    }
  }
  out
}

#' One section of the settings panel: its own settings, then the rest under
#' "More settings"
#' @noRd
.app_section_ui <- function(section,
                            controls,
                            start_code = character(0),
                            before = NULL) {
  in_section <- Filter(function(ctl) ctl$section == section$id, controls)
  own <- Filter(function(ctl) ctl$own, in_section)
  more <- Filter(function(ctl) !ctl$own, in_section)
  shiny::tags$section(
    class = "hm-section", `data-section` = section$id,
    `aria-labelledby` = paste0("hm-title-", section$id),
    hidden = if(section$id != "scale") NA,
    shiny::div(
      class = "hm-section-head",
      shiny::h2(id = paste0("hm-title-", section$id), section$title),
      if(length(in_section) > 0) {
        shiny::tags$button(type = "button", class = "hm-link hm-reset-section",
                           `data-section` = section$id, "Reset section")
      }
    ),
    before,
    .app_controls_ui(own, start_code),
    if(length(more) > 0) {
      shiny::tags$details(
        class = "hm-more",
        shiny::tags$summary(paste0("More settings (", length(more), ")")),
        .app_controls_ui(more, start_code)
      )
    }
  )
}

#' The start page: drop a file, or choose an example
#' @noRd
.app_start_ui <- function() {
  shiny::div(
    id = "hm-start", class = "hm-view",
    shiny::tags$header(
      class = "hm-top",
      shiny::img(src = "heatmapr-app/logo.png", alt = "HeatmapR logo",
                 class = "hm-logo"),
      shiny::span(class = "hm-name", "HeatmapR"),
      shiny::div(class = "hm-fill"),
      shiny::a(href = "https://dillonhammill.github.io/HeatmapR/reference/heat_map_app.html",
               target = "_blank", rel = "noopener", "How the app works")
    ),
    .app_strip(),
    shiny::tags$main(
      class = "hm-start-main",
      shiny::div(
        class = "hm-hero",
        shiny::img(src = "heatmapr-app/logo.png", alt = "", class = "hm-hero-logo"),
        shiny::div(
          shiny::h1("Build a heatmap"),
          shiny::p("Drop a table or a single-cell file, or start from an example. ",
                   "Every setting of ", shiny::tags$code("heat_map()"),
                   " is here, and the app writes the R code that draws your heatmap again.")
        )
      ),
      shiny::tags$section(
        class = "hm-drop", `aria-label` = "Upload a file",
        .app_icon("upload", 44),
        shiny::h2("Drop a file here"),
        shiny::p("CSV, TSV or text tables, Excel, R data (.rds) or AnnData (.h5ad)"),
        shiny::fileInput(
          "file", NULL, buttonLabel = "Choose a file",
          placeholder = "or drop it here",
          accept = c(".csv", ".tsv", ".tab", ".txt", ".xlsx", ".xls", ".rds",
                     ".h5ad"),
          width = "100%"
        )
      ),
      shiny::tags$section(
        `aria-labelledby` = "hm-examples-title",
        shiny::h2(id = "hm-examples-title", class = "hm-h2",
                  "Or start from an example"),
        shiny::div(
          class = "hm-examples",
          lapply(.APP_EXAMPLE_CARDS, function(ex) {
            shiny::actionLink(
              paste0("example_", ex$id), class = "hm-example",
              label = shiny::tagList(
                shiny::img(src = paste0("heatmapr-app/", ex$img), alt = ""),
                shiny::span(class = "hm-example-title", ex$title),
                shiny::span(class = "hm-example-text", ex$text)
              )
            )
          })
        )
      ),
      shiny::p(class = "hm-from-r", "Starting from data in R?",
               shiny::tags$code('heat_map_app(my_data, scale = "column")'),
               "opens here with your data and settings.")
    )
  )
}

#' The page of the app
#'
#' @param controls the settings of heat_map() (from .app_controls()).
#' @param start_code the starting settings as R code.
#' @param start whether the app opens on its start page.
#' @noRd
.app_ui <- function(controls,
                    start_code = character(0),
                    start = TRUE) {
  sections <- Filter(function(s) s$id != "fun", .APP_SECTIONS)
  data_before <- shiny::tagList(
    shiny::uiOutput("data_card"),
    shiny::uiOutput("data_roles"),
    shiny::conditionalPanel(
      "output.has_assays",
      shiny::selectInput("assay", shiny::tagList(
        shiny::span(class = "hm-lab", "Values"),
        shiny::tags$code(class = "hm-arg", "assay")),
        choices = c("Default" = ""), width = "100%", selectize = FALSE)
    ),
    shiny::div(
      class = "hm-replace",
      shiny::fileInput("file2", "Another file", buttonLabel = "Choose",
                       accept = c(".csv", ".tsv", ".tab", ".txt", ".xlsx",
                                  ".xls", ".rds", ".h5ad"),
                       width = "100%"),
      shiny::selectInput("example", "or an example",
                         choices = c("Choose" = "",
                                     stats::setNames(
                                       vapply(.APP_EXAMPLE_CARDS,
                                              function(e) e$id, ""),
                                       vapply(.APP_EXAMPLE_CARDS,
                                              function(e) e$title, ""))),
                         width = "100%", selectize = FALSE)
    )
  )
  section_ui <- lapply(sections, function(s) {
    .app_section_ui(s, controls, start_code,
                    before = if(s$id == "data") data_before)
  })
  fun_section <- shiny::tags$section(
    class = "hm-section", `data-section` = "fun",
    `aria-labelledby` = "hm-title-fun", hidden = NA,
    shiny::div(class = "hm-section-head",
               shiny::h2(id = "hm-title-fun", shiny::textOutput("fun_title",
                                                                inline = TRUE))),
    shiny::uiOutput("fun_settings")
  )
  rail <- shiny::tags$nav(
    class = "hm-rail", `aria-label` = "Settings sections",
    lapply(.APP_SECTIONS, function(s) {
      shiny::tags$button(
        type = "button", class = "hm-rail-item", `data-section` = s$id,
        `aria-pressed` = if(s$id == "scale") "true" else "false",
        hidden = if(s$id == "fun") NA,
        .app_icon(s$id), shiny::span(class = "hm-rail-label",
                                     if(s$id == "fun") shiny::textOutput("fun_rail", inline = TRUE) else s$rail),
        shiny::span(class = "hm-badge", `data-section` = s$id)
      )
    }),
    shiny::div(class = "hm-fill"),
    shiny::tags$button(type = "button", class = "hm-rail-item hm-rail-all",
                       `data-section` = "all", `aria-pressed` = "false",
                       .app_icon("all"),
                       shiny::span(class = "hm-rail-label",
                                   paste("All", length(controls), "settings")))
  )
  panel <- shiny::div(
    class = "hm-panel", id = "hm-panel",
    shiny::div(
      class = "hm-find",
      .app_icon("search", 18),
      shiny::tags$label(`for` = "find", class = "hm-sr", "Find a setting"),
      shiny::tags$input(id = "find", type = "search", autocomplete = "off",
                        placeholder = "Find a setting, e.g. border")
    ),
    shiny::p(class = "hm-find-none", hidden = NA,
             "No setting matches. Try another word, such as colour or label."),
    fun_section,
    section_ui
  )
  top <- shiny::tags$header(
    class = "hm-top",
    shiny::div(class = "hm-brand",
               shiny::img(src = "heatmapr-app/logo.png", alt = "HeatmapR logo",
                          class = "hm-logo"),
               shiny::span(class = "hm-name", "HeatmapR")),
    shiny::tags$button(type = "button", class = "hm-chip-data", id = "data_chip",
                       .app_icon("data", 18),
                       shiny::uiOutput("data_chip_text", inline = TRUE)),
    shiny::div(
      class = "hm-seg hm-kind",
      shiny::radioButtons("fun", NULL,
                          choices = c("Heatmap" = "heat_map",
                                      "Dot plot" = "heat_map_dot",
                                      "Correlation" = "heat_map_cor"),
                          inline = TRUE)
    ),
    shiny::div(class = "hm-fill"),
    shiny::div(
      class = "hm-actions",
      shiny::actionButton("undo", label = .app_icon("undo", 18),
                          class = "hm-icon-btn", title = "Undo",
                          `aria-label` = "Undo", disabled = NA),
      shiny::actionButton("redo", label = .app_icon("redo", 18),
                          class = "hm-icon-btn", title = "Redo",
                          `aria-label` = "Redo", disabled = NA),
      shiny::tags$button(type = "button", id = "code_toggle", class = "hm-btn",
                         `aria-pressed` = "true", `aria-controls` = "hm-code",
                         .app_icon("code", 18), "R code"),
      shiny::actionButton("download_open",
                          label = shiny::tagList(.app_icon("download", 18),
                                                 "Download"),
                          class = "hm-btn hm-primary")
    )
  )
  canvas <- shiny::tags$main(
    class = "hm-main", id = "hm-main",
    shiny::uiOutput("changed"),
    shiny::uiOutput("issue"),
    shiny::div(
      class = "hm-card",
      shiny::div(
        class = "hm-toolbar",
        shiny::div(
          class = "hm-zoom", role = "group", `aria-label` = "Zoom",
          shiny::tags$button(type = "button", id = "hm-zoom-out",
                             `aria-label` = "Zoom out", shiny::HTML("&minus;")),
          shiny::tags$button(type = "button", id = "hm-zoom-fit", "Fit"),
          shiny::tags$button(type = "button", id = "hm-zoom-in",
                             `aria-label` = "Zoom in", "+")
        ),
        shiny::selectInput(
          "size", NULL,
          choices = c("Square, 7 in" = "7x7",
                      "Journal column, 3.5 in" = "3.5x3.5",
                      "Journal page, 7.2 by 6 in" = "7.2x6",
                      "Slide 16:9, 13.33 by 7.5 in" = "13.33x7.5",
                      "A4, 8.27 by 11.69 in" = "8.27x11.69",
                      "Your own size" = "custom"),
          width = "215px", selectize = FALSE
        ),
        shiny::conditionalPanel(
          "input.size == 'custom'", class = "hm-size-custom",
          shiny::numericInput("width", "Width (in)", 7, min = 1, max = 60,
                              step = 0.5, width = "92px"),
          shiny::numericInput("height", "Height (in)", 7, min = 1, max = 60,
                              step = 0.5, width = "92px")
        ),
        shiny::div(class = "hm-fill"),
        shiny::tags$label(class = "hm-toggle",
                          shiny::tags$input(type = "checkbox", id = "hm-hover",
                                            checked = NA),
                          "Values on hover")
      ),
      shiny::div(
        id = "hm-canvas", class = "hm-canvas",
        shiny::div(id = "hm-stage", class = "hm-stage",
                   shiny::img(id = "hm-img", alt = "The heatmap"),
                   shiny::div(id = "hm-overlay", class = "hm-overlay")),
        shiny::p(id = "hm-empty", class = "hm-empty", "Drawing the heatmap..."),
        shiny::div(id = "hm-tip", class = "hm-tip", role = "tooltip", hidden = NA)
      ),
      shiny::div(
        id = "hm-selbar", class = "hm-selbar", hidden = NA,
        shiny::span(id = "hm-selcount"),
        shiny::actionButton("sel_show", "Show only these", class = "hm-btn hm-small"),
        shiny::actionButton("sel_label", "Label them", class = "hm-btn hm-small"),
        shiny::tags$button(type = "button", id = "hm-sel-copy",
                           class = "hm-btn hm-small", "Copy names"),
        shiny::tags$button(type = "button", id = "hm-sel-clear",
                           class = "hm-link", "Clear")
      ),
      shiny::uiOutput("status")
    )
  )
  code_panel <- shiny::tags$aside(
    class = "hm-code", id = "hm-code", `aria-labelledby` = "hm-code-title",
    shiny::div(
      class = "hm-code-head",
      shiny::h2(id = "hm-code-title", "R code"),
      shiny::tags$button(type = "button", id = "hm-code-copy",
                         class = "hm-btn hm-small hm-dark", "Copy"),
      shiny::downloadButton("code_download", "Save .R",
                            class = "hm-btn hm-small hm-dark", icon = NULL)
    ),
    shiny::p(class = "hm-code-note",
             "Draws this heatmap again in R. Only the settings you changed are written."),
    shiny::tags$pre(id = "hm-code-text", class = "hm-code-text"),
    shiny::p(class = "hm-code-foot", "The highlighted line is the last change.")
  )
  work <- shiny::div(
    id = "hm-work", class = "hm-view",
    top, .app_strip(),
    shiny::div(class = "hm-body", rail, panel, canvas, code_panel)
  )
  bslib::page(
    title = "HeatmapR",
    theme = bslib::bs_theme(
      version = 5, primary = "#46327E", secondary = "#5E5D72",
      base_font = bslib::font_collection("Inter", "system-ui", "-apple-system",
                                         "Segoe UI", "Roboto", "sans-serif"),
      code_font = bslib::font_collection("JetBrains Mono", "ui-monospace",
                                         "Menlo", "Consolas", "monospace")
    ),
    shiny::tags$head(
      shiny::tags$link(rel = "icon", type = "image/png",
                       href = "heatmapr-app/logo.png"),
      shiny::tags$link(rel = "stylesheet", href = "heatmapr-app/app.css"),
      shiny::tags$script(src = "heatmapr-app/app.js")
    ),
    shiny::div(
      class = paste("hm-app", if(start) "hm-on-start" else "hm-on-work"),
      id = "hm-app",
      .app_start_ui(), work
    )
  )
}
