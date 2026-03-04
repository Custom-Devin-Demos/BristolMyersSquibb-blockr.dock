$(function () {
  'use strict';

  var svgNS = 'http://www.w3.org/2000/svg';
  var svgContainer = null;
  var currentLinks = [];
  var currentMeta = {};
  var resizeObserver = null;
  var mutationObserver = null;
  var rafId = null;

  // Category color map matching R/block-meta.R
  var categoryColors = {
    input: '#0072B2',
    transform: '#009E73',
    structured: '#56B4E9',
    plot: '#E69F00',
    table: '#CC79A7',
    model: '#F0E442',
    output: '#D55E00',
    utility: '#CCCCCC'
  };
  var defaultColor = '#999999';

  function getColorForCategory(category) {
    if (!category) return defaultColor;
    return categoryColors[category] || defaultColor;
  }

  function ensureSvgContainer() {
    if (svgContainer && document.body.contains(svgContainer)) {
      return svgContainer;
    }

    svgContainer = document.createElementNS(svgNS, 'svg');
    svgContainer.setAttribute('class', 'blockr-connection-lines');
    svgContainer.setAttribute('width', '100%');
    svgContainer.setAttribute('height', '100%');

    // Add arrowhead marker definitions
    var defs = document.createElementNS(svgNS, 'defs');
    svgContainer.appendChild(defs);

    document.body.appendChild(svgContainer);
    return svgContainer;
  }

  function ensureArrowMarker(color, markerId) {
    var svg = ensureSvgContainer();
    var defs = svg.querySelector('defs');
    var existing = defs.querySelector('#' + markerId);
    if (existing) return;

    var marker = document.createElementNS(svgNS, 'marker');
    marker.setAttribute('id', markerId);
    marker.setAttribute('viewBox', '0 0 10 7');
    marker.setAttribute('refX', '10');
    marker.setAttribute('refY', '3.5');
    marker.setAttribute('markerWidth', '8');
    marker.setAttribute('markerHeight', '6');
    marker.setAttribute('orient', 'auto-start-reverse');

    var polygon = document.createElementNS(svgNS, 'polygon');
    polygon.setAttribute('points', '0 0, 10 3.5, 0 7');
    polygon.setAttribute('fill', color);
    polygon.setAttribute('opacity', '0.7');

    marker.appendChild(polygon);
    defs.appendChild(marker);
  }

  function getPanelElement(blockId) {
    // Dock panels use the format: {ns}-dock-block_panel-{blockId}
    // We search for elements whose id ends with block_panel-{blockId}
    var el = document.querySelector(
      '[id$="dock-block_panel-' + blockId + '"]'
    );
    if (el) return el;

    // Fallback: search for the panel tab header
    el = document.querySelector(
      '[id$="block_panel-' + blockId + '"]'
    );
    return el;
  }

  function getPanelRect(blockId) {
    var el = getPanelElement(blockId);
    if (!el) return null;
    return el.getBoundingClientRect();
  }

  function computePath(fromRect, toRect) {
    // Connect from right edge of source to left edge of target
    var fromX, fromY, toX, toY;

    var fromCenterX = fromRect.left + fromRect.width / 2;
    var fromCenterY = fromRect.top + fromRect.height / 2;
    var toCenterX = toRect.left + toRect.width / 2;
    var toCenterY = toRect.top + toRect.height / 2;

    var dx = toCenterX - fromCenterX;
    var dy = toCenterY - fromCenterY;

    // Determine which edges to connect based on relative positions
    if (Math.abs(dx) > Math.abs(dy)) {
      // Horizontal connection
      if (dx > 0) {
        fromX = fromRect.right;
        fromY = fromCenterY;
        toX = toRect.left;
        toY = toCenterY;
      } else {
        fromX = fromRect.left;
        fromY = fromCenterY;
        toX = toRect.right;
        toY = toCenterY;
      }
    } else {
      // Vertical connection
      if (dy > 0) {
        fromX = fromCenterX;
        fromY = fromRect.bottom;
        toX = toCenterX;
        toY = toRect.top;
      } else {
        fromX = fromCenterX;
        fromY = fromRect.top;
        toX = toCenterX;
        toY = toRect.bottom;
      }
    }

    // Create a smooth cubic bezier curve
    var midX = (fromX + toX) / 2;
    var midY = (fromY + toY) / 2;

    var cpOffset;
    if (Math.abs(dx) > Math.abs(dy)) {
      cpOffset = Math.min(Math.abs(dx) * 0.5, 80);
      var cp1x = fromX + (dx > 0 ? cpOffset : -cpOffset);
      var cp1y = fromY;
      var cp2x = toX - (dx > 0 ? cpOffset : -cpOffset);
      var cp2y = toY;
      return (
        'M ' + fromX + ' ' + fromY +
        ' C ' + cp1x + ' ' + cp1y +
        ', ' + cp2x + ' ' + cp2y +
        ', ' + toX + ' ' + toY
      );
    } else {
      cpOffset = Math.min(Math.abs(dy) * 0.5, 80);
      var cp1x = fromX;
      var cp1y = fromY + (dy > 0 ? cpOffset : -cpOffset);
      var cp2x = toX;
      var cp2y = toY - (dy > 0 ? cpOffset : -cpOffset);
      return (
        'M ' + fromX + ' ' + fromY +
        ' C ' + cp1x + ' ' + cp1y +
        ', ' + cp2x + ' ' + cp2y +
        ', ' + toX + ' ' + toY
      );
    }
  }

  function drawConnections() {
    var svg = ensureSvgContainer();

    // Clear existing paths (keep defs)
    var paths = svg.querySelectorAll('path');
    for (var i = 0; i < paths.length; i++) {
      paths[i].remove();
    }

    if (!currentLinks || currentLinks.length === 0) return;

    for (var j = 0; j < currentLinks.length; j++) {
      var link = currentLinks[j];
      var fromRect = getPanelRect(link.from);
      var toRect = getPanelRect(link.to);

      if (!fromRect || !toRect) continue;

      // Determine color from source block category
      var category = currentMeta[link.from]
        ? currentMeta[link.from].category
        : null;
      var color = getColorForCategory(category);
      var markerId = 'arrow-' + color.replace('#', '');

      ensureArrowMarker(color, markerId);

      var pathD = computePath(fromRect, toRect);

      // Background path (wider, for glow effect)
      var bgPath = document.createElementNS(svgNS, 'path');
      bgPath.setAttribute('d', pathD);
      bgPath.setAttribute('fill', 'none');
      bgPath.setAttribute('stroke', color);
      bgPath.setAttribute('stroke-width', '4');
      bgPath.setAttribute('stroke-opacity', '0.12');
      bgPath.setAttribute('stroke-linecap', 'round');
      svg.appendChild(bgPath);

      // Main path with animated dashes
      var path = document.createElementNS(svgNS, 'path');
      path.setAttribute('d', pathD);
      path.setAttribute('fill', 'none');
      path.setAttribute('stroke', color);
      path.setAttribute('stroke-width', '2');
      path.setAttribute('stroke-opacity', '0.6');
      path.setAttribute('stroke-dasharray', '8 4');
      path.setAttribute('stroke-linecap', 'round');
      path.setAttribute('marker-end', 'url(#' + markerId + ')');
      path.setAttribute('class', 'blockr-connection-path');
      svg.appendChild(path);
    }
  }

  function scheduleRedraw() {
    if (rafId) cancelAnimationFrame(rafId);
    rafId = requestAnimationFrame(function () {
      rafId = null;
      drawConnections();
    });
  }

  function setupObservers() {
    // Watch for DOM changes (panels being added/removed/moved)
    if (mutationObserver) mutationObserver.disconnect();
    mutationObserver = new MutationObserver(function () {
      scheduleRedraw();
    });
    mutationObserver.observe(document.body, {
      childList: true,
      subtree: true,
      attributes: true,
      attributeFilter: ['style', 'class']
    });

    // Watch for resize events
    window.addEventListener('resize', scheduleRedraw);

    // Observe the dock container for size changes
    if (resizeObserver) resizeObserver.disconnect();
    resizeObserver = new ResizeObserver(function () {
      scheduleRedraw();
    });

    // Observe panels and the dock container
    var dockEl = document.querySelector('.dv-dockview');
    if (dockEl) {
      resizeObserver.observe(dockEl);
    }

    // Also listen to mouse events for drag operations
    document.addEventListener('mouseup', function () {
      setTimeout(scheduleRedraw, 100);
    });
  }

  function normalizeLinks(links) {
    if (!links) return [];
    // If links is an array, use it directly
    if (Array.isArray(links)) return links;
    // If links is a named object (from R named list), convert to array
    var result = [];
    var keys = Object.keys(links);
    for (var i = 0; i < keys.length; i++) {
      result.push(links[keys[i]]);
    }
    return result;
  }

  // Shiny custom message handler
  Shiny.addCustomMessageHandler(
    'update-connection-lines',
    function (message) {
      currentLinks = normalizeLinks(message.links);
      currentMeta = message.meta || {};
      ensureSvgContainer();
      setupObservers();
      scheduleRedraw();
    }
  );
});
