css_modal <- function() {
  tags$style(
    HTML(
      "#shiny-modal .modal-header {
        padding: 12px 20px;
        border-bottom: 1px solid #e2e8f0;
      }
      #shiny-modal .modal-title {
        font-size: 1.125rem;
        font-weight: 600;
        margin: 0;
      }
      #shiny-modal .modal-body {
        padding: 20px;
      }
      #shiny-modal .modal-body .form-group {
        width: 100%;
        margin-bottom: 16px;
      }
      #shiny-modal .modal-body .selectize-input,
      #shiny-modal .modal-body input[type='text'] {
        width: 100%;
      }
      #shiny-modal .modal-body .shiny-input-container {
        width: 100%;
      }
      #shiny-modal .modal-body .control-label {
        font-size: 0.875rem;
        color: #6c757d;
        margin-bottom: 4px;
        font-weight: normal;
      }
      #shiny-modal .modal-footer {
        padding: 12px 20px;
        border-top: 1px solid #e2e8f0;
        gap: 8px;
      }
      #shiny-modal .modal-footer .btn {
        font-size: 0.875rem;
        padding: 0.375rem 0.75rem;
      }"
    )
  )
}

css_modal_advanced <- function(id) {
  tagList(
    css_modal(),
    tags$style(
      HTML(
        sprintf(
          "#%s {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
          }
          #%s.expanded {
            max-height: 500px;
            overflow: visible;
            transition: max-height 0.5s ease-in;
          }
          .modal-advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 8px 0;
            margin-bottom: 8px;
            display: flex;
            align-items: center;
            gap: 6px;
            color: #6c757d;
            font-size: 0.875rem;
          }
          .modal-chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 14px;
            font-weight: bold;
          }
          .modal-chevron.rotated {
            transform: rotate(90deg);
          }",
          id,
          id
        )
      )
    )
  )
}

css_block_selectize <- function() {
  tags$style(
    HTML(
      ".selectize-dropdown-content {
        max-height: 450px !important;
        padding: 8px 0;
      }
      .block-option {
        padding: 16px 24px;
        display: flex;
        align-items: flex-start;
        gap: 16px;
        margin: 4px 8px;
        border-radius: 6px;
        transition: background-color 0.15s ease;
      }
      .block-icon-wrapper {
        flex-shrink: 0;
        width: 40px;
        height: 40px;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 18px;
      }
      .block-content {
        flex: 1;
        min-width: 0;
      }
      .block-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 10px;
        margin-bottom: 4px;
      }
      .block-name {
        font-weight: 600;
        font-size: 15px;
        color: #212529;
        flex: 1;
      }
      .block-desc {
        font-size: 13px;
        color: #6c757d;
        line-height: 1.4;
      }
      .badge-two-tone {
        display: inline-block;
        padding: 0.125rem 0.375rem;
        font-size: 0.625rem;
        border-radius: 0.25rem;
        background-color: rgba(148, 163, 184, 0.1);
        color: rgba(100, 116, 139, 0.9);
        border: 1px solid rgba(100, 116, 139, 0.1);
        white-space: nowrap;
        flex-shrink: 0;
      }
      .selectize-dropdown .block-option {
        border-bottom: 1px solid #f0f0f0;
      }
      .selectize-dropdown .block-option:last-child {
        border-bottom: none;
      }
      .selectize-dropdown .block-option:hover,
      .selectize-dropdown .block-option.active {
        background-color: #e9ecef;
      }
      .selectize-input .remove {
        text-decoration: none !important;
        color: #6c757d !important;
        font-weight: normal !important;
        border: none !important;
        margin-left: 8px !important;
        padding: 2px 6px !important;
        border-radius: 3px !important;
        transition: background-color 0.2s ease, color 0.2s ease;
      }
      .selectize-input .remove:hover {
        background-color: rgba(108, 117, 125, 0.1) !important;
        color: #495057 !important;
      }"
    )
  )
}

js_blk_selectize_render <- function() {

  icon_style <- blockr_option("icon_style", "light")

  I(
    sprintf(
      "(
        function() {
          var iconStyle = '%s';

          // Shared helper functions
          var hexToRgba = function(hex, alpha) {
            var r = parseInt(hex.slice(1, 3), 16);
            var g = parseInt(hex.slice(3, 5), 16);
            var b = parseInt(hex.slice(5, 7), 16);
            return 'rgba(' + r + ', ' + g + ', ' + b + ', ' + alpha + ')';
          };

          var getIconColors = function(color) {
            if (iconStyle === 'light') {
              return {
                iconFill: color,
                bgColor: hexToRgba(color, 0.3)
              };
            } else {
              return {
                iconFill: 'white',
                bgColor: color
              };
            }
          };

          var styleIcon = function(iconSvg, color, size) {
            var colors = getIconColors(color);

            // Remove existing style attribute using string manipulation
            var cleanSvg = iconSvg;
            var styleStart = cleanSvg.indexOf('style=\"');
            if (styleStart !== -1) {
              var styleEnd = cleanSvg.indexOf('\"', styleStart + 7);
              if (styleEnd !== -1) {
                cleanSvg = cleanSvg.substring(0, styleStart) +
                           cleanSvg.substring(styleEnd + 1);
              }
            }

            return {
              svg: cleanSvg.replace(
                '<svg ',
                '<svg style=\"width: ' + size + 'px; height: ' + size +
                'px; fill: ' + colors.iconFill + ';\" '
              ),
              bgColor: colors.bgColor
            };
          };

          return {
            item: function(item, escape) {
              var name = escape(item.label);
              var pkg = escape(item.package || '');
              var color = item.color || '#6c757d';
              var iconSvg = item.icon || '';

              var styledIcon = styleIcon(iconSvg, color, 14);
              var styledSvg = styledIcon.svg;
              var bgColor = styledIcon.bgColor;

              var containerStyle =
                'display: inline-flex; align-items: center; gap: 8px; ' +
                'padding: 4px 8px; background-color: #f8f9fa; ' +
                'border-radius: 6px; border: 1px solid #e9ecef;';
              var iconWrapperStyle =
                'background-color: ' + bgColor + '; width: 24px; ' +
                'height: 24px; border-radius: 4px; display: flex; ' +
                'align-items: center; justify-content: center; flex-shrink: 0;';
              var pkgBadgeHtml = pkg ?
                '<div class=\"badge-two-tone\" style=\"margin-left: 4px;\">' +
                pkg + '</div>' : '';
              return '<div style=\"' + containerStyle + '\">' +
                     '<div style=\"' + iconWrapperStyle + '\">' +
                     styledSvg + '</div>' +
                     '<div style=\"font-weight: 500; font-size: 14px;\">' +
                     name + '</div>' + pkgBadgeHtml + '</div>';
            },
            option: function(item, escape) {
              var name = escape(item.label);
              var desc = escape(item.description || '');
              var pkg = escape(item.package || '');
              var color = item.color || '#6c757d';
              var iconSvg = item.icon || '';

              var styledIcon = styleIcon(iconSvg, color, 20);
              var styledSvg = styledIcon.svg;
              var bgColor = styledIcon.bgColor;

              var iconWrapperStyle = 'background-color: ' + bgColor + ';';
              var iconWrapper = '<div class=\"block-icon-wrapper\" ' +
                                'style=\"' + iconWrapperStyle + '\">' +
                                styledSvg + '</div>';

              var pkgBadge = pkg ?
                             '<div class=\"badge-two-tone\">' + pkg +
                             '</div>' : '';

              // For board blocks, show type/ID info as description
              // For registry blocks, show the description field
              var descHtml = '';
              if (item.block_type) {
                var blockType = escape(item.block_type);
                var blockId = escape(item.block_id || '');
                descHtml = '<div class=\"block-desc\">type: ' + blockType +
                           (blockId ? ' &middot; ID: ' + blockId : '') +
                           '</div>';
              } else if (desc) {
                descHtml = '<div class=\"block-desc\">' + desc + '</div>';
              }

              return '<div class=\"block-option\">' + iconWrapper +
                     '<div class=\"block-content\">' +
                     '<div class=\"block-header\">' +
                     '<div class=\"block-name\">' + name + '</div>' + pkgBadge +
                     '</div>' + descHtml + '</div>' + '</div>';
            }
          };
        }
      )()",
      icon_style
    )
  )
}

auto_focus_script <- function(id) {
  tags$script(
    HTML(
      sprintf(
        "$('#shiny-modal').on(
          'shown.bs.modal',
          function() {
            $('#%s')[0].selectize.focus();
          }
        );",
        id
      )
    )
  )
}

toggle_button <- function(opt_id, tog_id) {
  div(
    class = "modal-advanced-toggle text-muted",
    id = tog_id,
    onclick = sprintf(
      "const section = document.getElementById('%s');
      const chevron = document.querySelector('#%s .modal-chevron');
      section.classList.toggle('expanded');
      chevron.classList.toggle('rotated');",
      opt_id,
      tog_id
    ),
    tags$span(class = "modal-chevron", "\u203A"),
    "Show advanced options"
  )
}

confirm_button <- function(...) {
  div(
    style = "display: flex; justify-content: flex-end; margin-top: 20px;",
    actionButton(..., class = "btn-primary")
  )
}

block_browser_ui <- function(ns, selection_id,
                             blocks = list_blocks()) {

  meta <- registry_metadata(blocks)
  icon_style <- blockr_option("icon_style", "light")
  colors <- blk_color(meta[["category"]])

  # Map categories to user-friendly tab labels
  cat_labels <- c(
    input = "Data",
    transform = "Transform",
    plot = "Visualize",
    output = "Export"
  )

  categories <- unique(meta[["category"]])
  tab_categories <- categories[categories %in% names(cat_labels)]
  other_categories <- setdiff(categories, names(cat_labels))

  # Build tab buttons
  tab_all <- tags$button(
    class = "block-browser-tab active",
    `data-category` = "all",
    onclick = sprintf(
      "blockBrowserFilter('%s', 'all');",
      ns(selection_id)
    ),
    "All"
  )

  tab_buttons <- lapply(tab_categories, function(cat) {
    label <- cat_labels[[cat]]
    tags$button(
      class = "block-browser-tab",
      `data-category` = cat,
      onclick = sprintf(
        "blockBrowserFilter('%s', '%s');",
        ns(selection_id),
        cat
      ),
      label
    )
  })

  # Add "Other" tab if there are uncategorized blocks
  if (length(other_categories) > 0L) {
    tab_buttons <- c(tab_buttons, list(
      tags$button(
        class = "block-browser-tab",
        `data-category` = "other",
        onclick = sprintf(
          "blockBrowserFilter('%s', 'other');",
          ns(selection_id)
        ),
        "Other"
      )
    ))
  }

  # Build block cards
  block_cards <- mapply(
    function(id, name, desc, cat, pkg, icon_svg, color) {
      styled_icon <- style_icon_html(icon_svg, color, icon_style)
      # Determine effective category for filtering
      effective_cat <- if (cat %in% names(cat_labels)) cat else "other"
      div(
        class = "block-browser-card",
        `data-value` = id,
        `data-category` = effective_cat,
        `data-searchtext` = tolower(paste(name, desc, pkg)),
        onclick = sprintf(
          "blockBrowserSelect('%s', '%s', this);",
          ns(selection_id),
          id
        ),
        div(
          class = "block-browser-card-icon",
          style = sprintf("background-color: %s;", styled_icon$bg_color),
          HTML(styled_icon$svg)
        ),
        div(
          class = "block-browser-card-content",
          div(
            class = "block-browser-card-header",
            span(class = "block-browser-card-name", name),
            if (nchar(pkg) > 0L) {
              span(class = "badge-two-tone", pkg)
            }
          ),
          if (nchar(desc) > 0L) {
            div(class = "block-browser-card-desc", desc)
          }
        )
      )
    },
    meta[["id"]],
    meta[["name"]],
    meta[["description"]],
    meta[["category"]],
    meta[["package"]],
    meta[["icon"]],
    colors,
    SIMPLIFY = FALSE
  )

  # Hidden input to hold the selected block value
  hidden_input <- tags$input(
    type = "hidden",
    id = ns(selection_id),
    name = ns(selection_id),
    class = "block-browser-hidden-input"
  )

  search_icon_svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' ",
    "viewBox='0 0 16 16' fill='currentColor'>",
    "<path d='M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001",
    "c.03.04.062.078.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85",
    "a1.007 1.007 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1",
    " 11 0z'/></svg>"
  )

  tagList(
    hidden_input,
    js_block_browser(ns(selection_id)),
    div(
      class = "block-browser-search",
      span(class = "block-browser-search-icon", HTML(search_icon_svg)),
      tags$input(
        type = "text",
        placeholder = "Search blocks by name, description, or package...",
        oninput = sprintf(
          "blockBrowserSearch('%s', this.value);",
          ns(selection_id)
        )
      )
    ),
    div(
      class = "block-browser-tabs",
      tab_all,
      tab_buttons
    ),
    div(
      class = "block-browser-grid",
      id = paste0(ns(selection_id), "-grid"),
      block_cards,
      div(
        class = "block-browser-empty",
        id = paste0(ns(selection_id), "-empty"),
        style = "display: none;",
        "No blocks match your search."
      )
    )
  )
}

style_icon_html <- function(icon_svg, color, icon_style = "light") {

  if (icon_style == "light") {
    icon_fill <- color
    bg_color <- hex_to_rgba(color, 0.3)
  } else {
    icon_fill <- "white"
    bg_color <- color
  }

  # Remove existing style attribute
  clean_svg <- sub('style="[^"]*"', '', icon_svg)

  # Add fill color and size
  styled_svg <- sub(
    "<svg ",
    sprintf(
      '<svg style="width: 22px; height: 22px; fill: %s;" ',
      icon_fill
    ),
    clean_svg
  )

  list(svg = styled_svg, bg_color = bg_color)
}

js_block_browser <- function(input_id) {
  tags$script(
    HTML(
      sprintf(
        "(function() {
  var inputId = '%s';
  var currentCategory = 'all';
  var currentSearch = '';

  window.blockBrowserSelect = function(nsInputId, value, el) {
    var grid = el.closest('.block-browser-grid');
    var cards = grid.querySelectorAll('.block-browser-card');
    cards.forEach(function(c) { c.classList.remove('selected'); });
    el.classList.add('selected');
    var hiddenInput = document.getElementById(nsInputId);
    if (hiddenInput) {
      hiddenInput.value = value;
      $(hiddenInput).trigger('change');
    }
    Shiny.setInputValue(nsInputId, value);
  };

  window.blockBrowserFilter = function(nsInputId, category) {
    currentCategory = category;
    var gridEl = document.getElementById(nsInputId + '-grid');
    if (!gridEl) return;
    var tabContainer = gridEl.previousElementSibling;
    var tabs = tabContainer.querySelectorAll('.block-browser-tab');
    tabs.forEach(function(t) { t.classList.remove('active'); });
    var clickedTab = tabContainer.querySelector(
      '[data-category=\"' + category + '\"]'
    );
    if (clickedTab) clickedTab.classList.add('active');
    applyFilters(nsInputId);
  };

  window.blockBrowserSearch = function(nsInputId, searchText) {
    currentSearch = searchText.toLowerCase();
    applyFilters(nsInputId);
  };

  function applyFilters(nsInputId) {
    var grid = document.getElementById(nsInputId + '-grid');
    var empty = document.getElementById(nsInputId + '-empty');
    if (!grid) return;
    var cards = grid.querySelectorAll('.block-browser-card');
    var visibleCount = 0;
    cards.forEach(function(card) {
      var cat = card.getAttribute('data-category');
      var searchtext = card.getAttribute('data-searchtext') || '';
      var catMatch = (currentCategory === 'all' || cat === currentCategory);
      var searchMatch = (!currentSearch ||
        searchtext.indexOf(currentSearch) !== -1);
      if (catMatch && searchMatch) {
        card.style.display = '';
        visibleCount++;
      } else {
        card.style.display = 'none';
      }
    });
    if (empty) {
      empty.style.display = visibleCount === 0 ? '' : 'none';
    }
  }
})();

$(document).on('shown.bs.modal', '#shiny-modal', function() {
  var searchInput = document.querySelector(
    '.block-browser-search input'
  );
  if (searchInput) searchInput.focus();
});",
        input_id
      )
    )
  )
}
