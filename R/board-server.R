board_server_callback <- function(board, update, ..., session = get_session()) {

  initial_board <- isolate(board$board)

  exts <- as.list(dock_extensions(initial_board))

  actions <- unlst(
    c(
      list(board_actions(initial_board)),
      lapply(exts, board_actions)
    )
  )

  triggers <- action_triggers(actions)

  dock <- manage_dock(board, update, triggers, session)

  ext_res <- lapply(
    exts,
    extension_server,
    list(board = board, update = update, dock = dock, actions = triggers),
    list(...)
  )

  register_actions(actions, triggers, board, update, ext_res)

  manage_pipeline_bar(board, dock, session)

  c(
    list(dock = dock, actions = triggers),
    ext_res
  )
}

manage_dock <- function(board, update, actions, session = get_session()) {

  dock <- set_dock_view_output(session = session)

  input <- session$input

  if (get_log_level() >= debug_log_level) {
    observeEvent(
      input[[dock_input("active-group")]],
      {
        ag <- input[[dock_input("active-group")]] # nolint: object_usage_linter.
        log_debug("active group is now {ag}")
      }
    )
  }

  observeEvent(
    req(input[[dock_input("initialized")]]),
    {
      layout <- dock_layout(board$board)

      restore_dock(layout, dock)

      for (id in as_dock_panel_id(layout)) {
        if (is_block_panel_id(id)) {
          show_block_panel(id, add_panel = FALSE, proxy = dock)
        } else if (is_ext_panel_id(id)) {
          show_ext_panel(id, add_panel = FALSE, proxy = dock)
        } else {
          blockr_abort(
            "Unknown panel type {class(id)}.",
            class = "dock_panel_invalid"
          )
        }
      }
    },
    once = TRUE
  )

  n_panels <- reactiveVal(
    isolate(length(determine_active_views(dock_layout(board$board))))
  )

  observeEvent(
    req(input[[dock_input("n-panels")]]),
    n_panels(input[[dock_input("n-panels")]])
  )

  observeEvent(
    input[[dock_input("panel-to-remove")]],
    {
      id <- as_dock_panel_id(
        input[[dock_input("panel-to-remove")]]
      )

      if (is_block_panel_id(id)) {
        hide_block_panel(id, rm_panel = TRUE, proxy = dock)
        n_panels(n_panels() - 1L)
      } else if (is_ext_panel_id(id)) {
        hide_ext_panel(id, rm_panel = TRUE, proxy = dock)
        n_panels(n_panels() - 1L)
      } else {
        blockr_abort(
          "Unknown panel type {class(id)}.",
          class = "dock_panel_invalid"
        )
      }
    }
  )

  observeEvent(
    input[[dock_input("panel-to-add")]],
    suggest_panels_to_add(dock, board, session = session)
  )

  observeEvent(
    req(n_panels() == 0),
    suggest_panels_to_add(
      dock,
      board,
      actions[["add_block_action"]],
      panels = list(),
      session
    )
  )

  observeEvent(
    input$confirm_add,
    {
      req(input$add_dock_panel)

      pos <- list(
        referenceGroup = input[[dock_input("panel-to-add")]],
        direction = "within"
      )

      for (id in input$add_dock_panel) {

        if (grepl("^blk-", id)) {

          show_block_panel(
            board_blocks(board$board)[sub("^blk-", "", id)],
            add_panel = pos,
            proxy = dock
          )

          n_panels(n_panels() + 1L)

        } else if (grepl("^ext-", id)) {

          exts <- as.list(dock_extensions(board$board))

          show_ext_panel(
            exts[[sub("^ext-", "", id)]],
            add_panel = pos,
            proxy = dock
          )

          n_panels(n_panels() + 1L)

        } else {

          blockr_abort(
            "Unknown panel specification {id}.",
            class = "dock_panel_invalid"
          )
        }
      }

      removeModal()
    }
  )

  prev_active_group <- reactiveVal()
  active_group_trail <- reactiveVal()

  observeEvent(
    input[[dock_input("active-group")]],
    {
      cur_ag <- input[[dock_input("active-group")]]
      pre_ag <- active_group_trail()
      if (!identical(pre_ag, cur_ag)) {
        log_trace("setting previous active group to {pre_ag}")
        prev_active_group(pre_ag)
      }
      active_group_trail(cur_ag)
    }
  )

  observeEvent(
    update()$blocks$mod,
    {
      blks <- update()$blocks$mod

      for (id in names(blks)) {

        blk <- blks[[id]]
        new_name <- block_name(blk)
        blk_panel_id <- as_block_panel_id(id)

        old_title <- get_dock_panel(blk_panel_id, dock)$title

        if (new_name == old_title) {
          next
        }

        log_debug("setting panel title {blk_panel_id} to '{new_name}'")

        dockViewR::set_panel_title(
          dock,
          blk_panel_id,
          new_name
        )
      }
    }
  )

  list(
    layout = reactive(dockViewR::get_dock(dock)),
    proxy = dock,
    prev_active_group = prev_active_group
  )
}

suggest_panels_to_add <- function(dock, board, suggest_new = FALSE,
                                  panels = NULL, session = get_session()) {

  ns <- session$ns

  if (is.null(panels)) {
    panels <- dock_panel_ids(dock)
    if (length(panels) == 0L) {
      panels <- list()
    } else if (length(panels) == 1L) {
      panels <- list(panels)
    }
  }

  stopifnot(is.list(panels), all(lgl_ply(panels, is_dock_panel_id)))

  options_data <- list()

  # Get available blocks
  blk_opts <- setdiff(
    board_block_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_block_panel_id)])
  )

  if (length(blk_opts)) {
    blks <- board_blocks(board$board)[blk_opts]
    meta <- blks_metadata(blks)

    for (i in seq_along(blk_opts)) {
      id <- blk_opts[i]
      options_data[[length(options_data) + 1L]] <- list(
        value = paste0("blk-", id),
        label = block_name(blks[[id]]),
        description = paste0("ID: ", id),
        package = meta$package[i],
        icon = meta$icon[i],
        color = meta$color[i],
        searchtext = paste(block_name(blks[[id]]), id, meta$package[i])
      )
    }
  }

  # Get available extensions
  ext_opts <- setdiff(
    dock_ext_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_ext_panel_id)])
  )

  if (length(ext_opts)) {
    all_exts <- as.list(dock_extensions(board$board))

    for (ext_id in ext_opts) {
      ext <- all_exts[[ext_id]]
      ext_name <- extension_name(ext)
      ext_pkg <- ctor_pkg(extension_ctor(ext))

      options_data[[length(options_data) + 1L]] <- list(
        value = paste0("ext-", ext_id),
        label = ext_name,
        description = paste0("ID: ", ext_id),
        package = coal(ext_pkg, "local"),
        icon = extension_default_icon(),
        color = "#999999",
        searchtext = paste(ext_name, ext_id, ext_pkg)
      )
    }
  }

  if (length(options_data)) {
    showModal(
      modalDialog(
        title = "Add panel",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          css_modal(),
          css_block_selectize(),
          selectizeInput(
            ns("add_dock_panel"),
            label = "Select panel to add",
            choices = NULL,
            multiple = TRUE,
            options = list(
              options = options_data,
              valueField = "value",
              labelField = "label",
              searchField = c("label", "description", "searchtext"),
              placeholder = "Type to search...",
              openOnFocus = FALSE,
              plugins = list("remove_button"),
              render = js_blk_selectize_render()
            )
          ),
          confirm_button(ns("confirm_add"), label = "Add Panel"),
          auto_focus_script(ns("add_dock_panel"))
        )
      )
    )
  } else if (!isFALSE(suggest_new)) {
    suggest_new(TRUE)
  } else {
    notify("No further panels can be added. Remove some panels first.")
  }
}

extension_default_icon <- function() {
  as.character(bsicons::bs_icon("gear"))
}

manage_pipeline_bar <- function(board, dock, session = get_session()) {

  ns <- session$ns

  # Render pipeline chips as server-side UI
  session$output$pipeline_chips <- renderUI({
    brd <- board$board
    blks <- board_blocks(brd)
    lnks <- board_links(brd)
    err_ids <- tryCatch(
      pipeline_error_ids(board),
      error = function(e) character()
    )
    build_pipeline_ui(blks, lnks, err_ids)
  })

  # Handle chip click -> select block panel
  observeEvent(
    session$input$pipeline_chip_click,
    {
      block_id <- session$input$pipeline_chip_click$id
      req(block_id)
      tryCatch(
        select_block_panel(block_id, proxy = dock$proxy),
        error = function(e) NULL
      )
    }
  )

  invisible(NULL)
}

pipeline_error_ids <- function(board) {
  blk_names <- names(board$blocks)
  if (is.null(blk_names) || length(blk_names) == 0L) {
    return(character())
  }

  err_ids <- character()
  for (bid in blk_names) {
    blk_data <- board$blocks[[bid]]
    if (!is.null(blk_data$server$cond)) {
      cond <- tryCatch(
        reactiveValuesToList(blk_data$server$cond),
        error = function(e) list()
      )
      errs <- cond[["error"]]
      if (length(errs) && length(unlist(errs))) {
        err_ids <- c(err_ids, bid)
      }
    }
  }
  err_ids
}

pipeline_chip_color <- function(category) {
  switch(
    category,
    input = "#3b82f6",
    transform = "#d97706",
    plot = "#dc2626",
    output = "#0d9488",
    "#6b7280"
  )
}

pipeline_chip_icon <- function(category) {
  switch(
    category,
    input = paste0(
      "<svg viewBox='0 0 16 16' fill='currentColor'>",
      "<path d='M5.5 5.5A.5.5 0 0 1 6 6v6a.5.5 0 0 1-.5.5h-3",
      "a.5.5 0 0 1-.5-.5V6a.5.5 0 0 1 .5-.5h3zm-3-1A1.5 1.5 ",
      "0 0 0 1 6v6a1.5 1.5 0 0 0 1.5 1.5h3A1.5 1.5 0 0 0 7 ",
      "12V6a1.5 1.5 0 0 0-1.5-1.5h-3zm7 1A.5.5 0 0 1 10 6v6",
      "a.5.5 0 0 1-.5.5h-3A.5.5 0 0 1 6 12V6a.5.5 0 0 1 ",
      ".5-.5h3zm-3-1A1.5 1.5 0 0 0 5 6v6a1.5 1.5 0 0 0 1.5 ",
      "1.5h3A1.5 1.5 0 0 0 11 12V6a1.5 1.5 0 0 0-1.5-1.5h-3z",
      "M14 4a1 1 0 0 1 1 1v6a1 1 0 0 1-2 0V5a1 1 0 0 1 1-1z",
      "M2 4a1 1 0 0 1 1 1v6a1 1 0 0 1-2 0V5a1 1 0 0 1 1-1z'/>",
      "</svg>"
    ),
    transform = paste0(
      "<svg viewBox='0 0 16 16' fill='currentColor'>",
      "<path fill-rule='evenodd' d='M1 11.5a.5.5 0 0 0 ",
      ".5.5h11.793l-3.147 3.146a.5.5 0 0 0 .708.708l4-4a",
      ".5.5 0 0 0 0-.708l-4-4a.5.5 0 0 0-.708.708L13.293 ",
      "11H1.5a.5.5 0 0 0-.5.5zm14-7a.5.5 0 0 1-.5.5H2.707",
      "l3.147 3.146a.5.5 0 1 1-.708.708l-4-4a.5.5 0 0 1 ",
      "0-.708l4-4a.5.5 0 1 1 .708.708L2.707 4H14.5a.5.5 ",
      "0 0 1 .5.5z'/></svg>"
    ),
    plot = paste0(
      "<svg viewBox='0 0 16 16' fill='currentColor'>",
      "<path d='M4 11H2v3h2v-3zm5-4H7v7h2V7zm5-5h-2v12h2",
      "V2zm-2-1a1 1 0 0 0-1 1v12a1 1 0 0 0 1 1h2a1 1 0 ",
      "0 0 1-1V2a1 1 0 0 0-1-1h-2zM6 7a1 1 0 0 1 1-1h2a1",
      " 1 0 0 1 1 1v7a1 1 0 0 1-1 1H7a1 1 0 0 1-1-1V7zm",
      "-5 4a1 1 0 0 1 1-1h2a1 1 0 0 1 1 1v3a1 1 0 0 1-1 ",
      "1H2a1 1 0 0 1-1-1v-3z'/></svg>"
    ),
    output = paste0(
      "<svg viewBox='0 0 16 16' fill='currentColor'>",
      "<path d='M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 ",
      "1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 ",
      "0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z'/>",
      "<path d='M7.646 11.854a.5.5 0 0 0 .708 0l3-3a.5.5 ",
      "0 0 0-.708-.708L8.5 10.293V1.5a.5.5 0 0 0-1 0v8.793",
      "L5.354 8.146a.5.5 0 1 0-.708.708l3 3z'/></svg>"
    ),
    paste0(
      "<svg viewBox='0 0 16 16' fill='currentColor'>",
      "<path d='M9.405 1.05c-.413-1.4-2.397-1.4-2.81 0l-.1",
      ".34a1.464 1.464 0 0 1-2.105.872l-.31-.17c-1.283-.698",
      "-2.686.705-1.987 1.987l.169.311c.446.82.023 1.841-",
      ".872 2.105l-.34.1c-1.4.413-1.4 2.397 0 2.81l.34.1a",
      "1.464 1.464 0 0 1 .872 2.105l-.17.31c-.698 1.283",
      ".705 2.686 1.987 1.987l.311-.169a1.464 1.464 0 0 1 ",
      "2.105.872l.1.34c.413 1.4 2.397 1.4 2.81 0l.1-.34a",
      "1.464 1.464 0 0 1 2.105-.872l.31.17c1.283.698 ",
      "2.686-.705 1.987-1.987l-.169-.311a1.464 1.464 0 0 1 ",
      ".872-2.105l.34-.1c1.4-.413 1.4-2.397 0-2.81l-.34-.1",
      "a1.464 1.464 0 0 1-.872-2.105l.17-.31c.698-1.283-",
      ".705-2.686-1.987-1.987l-.311.169a1.464 1.464 0 0 1-",
      "2.105-.872l-.1-.34zM8 10.93a2.929 2.929 0 1 1 ",
      "0-5.86 2.929 2.929 0 0 1 0 5.858z'/></svg>"
    )
  )
}

#' @noRd
arrow_svg <- function() {
  paste0(
    "<svg viewBox='0 0 16 16' fill='currentColor' ",
    "width='14' height='14'>",
    "<path fill-rule='evenodd' d='M4.646 1.646a.5.5 0 0 1 ",
    ".708 0l6 6a.5.5 0 0 1 0 .708l-6 6a.5.5 0 0 1-.708-",
    ".708L10.293 8 4.646 2.354a.5.5 0 0 1 0-.708z'/></svg>"
  )
}

build_pipeline_ui <- function(blocks, links, error_ids = character()) {

  if (length(blocks) == 0L) {
    return(NULL)
  }

  blk_ids <- names(blocks)

  # Build adjacency from links
  edges <- list()
  targets <- character()
  for (lnk in links) {
    from_id <- lnk[["from"]]
    to_id <- lnk[["to"]]
    if (from_id %in% blk_ids && to_id %in% blk_ids) {
      edges[[length(edges) + 1L]] <- list(from = from_id, to = to_id)
      targets <- c(targets, to_id)
    }
  }

  # Topological order: start with roots (blocks not targeted by any link)
  roots <- setdiff(blk_ids, targets)
  if (length(roots) == 0L) roots <- blk_ids[1L]

  ordered <- character()
  visited <- character()
  queue <- roots

  while (length(queue) > 0L) {
    current <- queue[1L]
    queue <- queue[-1L]
    if (current %in% visited) next
    visited <- c(visited, current)
    ordered <- c(ordered, current)
    children <- character()
    for (e in edges) {
      if (e$from == current) children <- c(children, e$to)
    }
    queue <- c(queue, children)
  }

  # Add any blocks not reachable from roots
  remaining <- setdiff(blk_ids, ordered)
  ordered <- c(ordered, remaining)

  # Build set of connected pairs for arrows
  connected_pairs <- character()
  for (e in edges) {
    connected_pairs <- c(connected_pairs, paste0(e$from, "->", e$to))
  }

  meta <- blks_metadata(blocks)
  chip_tags <- list()

  for (i in seq_along(ordered)) {
    bid <- ordered[i]
    idx <- match(bid, blk_ids)
    blk <- blocks[[bid]]
    name <- block_name(blk)
    category <- meta$category[idx]
    color <- pipeline_chip_color(category)
    icon_html <- pipeline_chip_icon(category)
    chip_cls <- "blockr-pipeline-chip"
    if (bid %in% error_ids) chip_cls <- paste(chip_cls, "has-error")

    # Add arrow before chip if linked from previous block in order
    if (i > 1L) {
      prev_bid <- ordered[i - 1L]
      pair_key <- paste0(prev_bid, "->", bid)
      if (pair_key %in% connected_pairs) {
        chip_tags <- c(chip_tags, list(
          span(
            class = "blockr-pipeline-arrow",
            HTML(arrow_svg())
          )
        ))
      } else {
        chip_tags <- c(chip_tags, list(
          span(
            class = "blockr-pipeline-arrow",
            style = "color: var(--blockr-grey-300);",
            HTML("&middot;")
          )
        ))
      }
    }

    chip_tags <- c(chip_tags, list(
      span(
        class = chip_cls,
        `data-block-id` = bid,
        style = paste0("background-color: ", color, ";"),
        title = paste0(name, " (", category, ")"),
        HTML(icon_html),
        name
      )
    ))
  }

  tagList(chip_tags)
}
