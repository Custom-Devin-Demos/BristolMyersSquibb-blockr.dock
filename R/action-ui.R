css_modal <- function() {
  NULL
}

css_modal_advanced <- function(id) {
  NULL
}

css_block_selectize <- function() {
  NULL
}

js_blk_selectize_render <- function() {

  icon_style <- blockr_option("icon_style", "light")

  I(
    sprintf(
      "(
        function() {
          var iconStyle = '%s';
          var defaultColor = getComputedStyle(document.documentElement)
            .getPropertyValue('--blockr-color-text-muted')
            .trim() || '#6c757d';

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
                '<svg width=\"' + size + '\" height=\"' + size +
                '\" fill=\"' + colors.iconFill + '\" '
              ),
              bgColor: colors.bgColor
            };
          };

          return {
            item: function(item, escape) {
              var name = escape(item.label);
              var pkg = escape(item.package || '');
              var color = item.color || defaultColor;
              var iconSvg = item.icon || '';

              var styledIcon = styleIcon(iconSvg, color, 14);
              var styledSvg = styledIcon.svg;
              var bgColor = styledIcon.bgColor;

              var iconWrapperStyle = '--blockr-icon-bg: ' + bgColor + ';';
              var pkgBadgeHtml = pkg ?
                '<div class=\"badge-two-tone\">' + pkg + '</div>' : '';
              return '<div class=\"blockr-selectize-item\">' +
                     '<div class=\"blockr-selectize-item-icon\" style=\"' +
                     iconWrapperStyle + '\">' +
                     styledSvg + '</div>' +
                     '<div class=\"blockr-selectize-item-name\">' +
                     name + '</div>' + pkgBadgeHtml + '</div>';
            },
            option: function(item, escape) {
              var name = escape(item.label);
              var desc = escape(item.description || '');
              var pkg = escape(item.package || '');
              var color = item.color || defaultColor;
              var iconSvg = item.icon || '';

              var styledIcon = styleIcon(iconSvg, color, 20);
              var styledSvg = styledIcon.svg;
              var bgColor = styledIcon.bgColor;

              var iconWrapperStyle = '--blockr-icon-bg: ' + bgColor + ';';
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
    class = "blockr-confirm-btn-wrapper",
    actionButton(..., class = "btn-primary")
  )
}
