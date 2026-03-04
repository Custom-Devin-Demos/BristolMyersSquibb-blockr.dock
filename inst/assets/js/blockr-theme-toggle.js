(function() {
  'use strict';

  var STORAGE_KEY = 'blockr-theme';
  var DARK = 'dark';
  var LIGHT = 'light';

  // SVG icons
  var sunIcon = '<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="12" r="5"/><line x1="12" y1="1" x2="12" y2="3"/><line x1="12" y1="21" x2="12" y2="23"/><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/><line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/><line x1="1" y1="12" x2="3" y2="12"/><line x1="21" y1="12" x2="23" y2="12"/><line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/><line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/></svg>';
  var moonIcon = '<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"/></svg>';

  function getStoredTheme() {
    try {
      return localStorage.getItem(STORAGE_KEY);
    } catch (e) {
      return null;
    }
  }

  function setStoredTheme(theme) {
    try {
      localStorage.setItem(STORAGE_KEY, theme);
    } catch (e) {
      // localStorage not available
    }
  }

  function getPreferredTheme() {
    var stored = getStoredTheme();
    if (stored === DARK || stored === LIGHT) {
      return stored;
    }
    // Respect system preference
    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
      return DARK;
    }
    return LIGHT;
  }

  function applyTheme(theme) {
    document.documentElement.setAttribute('data-theme', theme);
    setStoredTheme(theme);
    updateToggleButton(theme);
  }

  function updateToggleButton(theme) {
    var btn = document.querySelector('.blockr-theme-toggle');
    if (!btn) return;
    btn.innerHTML = theme === DARK ? sunIcon : moonIcon;
    btn.setAttribute('title', theme === DARK ? 'Switch to light mode' : 'Switch to dark mode');
    btn.setAttribute('aria-label', theme === DARK ? 'Switch to light mode' : 'Switch to dark mode');
  }

  function toggleTheme() {
    var current = document.documentElement.getAttribute('data-theme') || LIGHT;
    var next = current === DARK ? LIGHT : DARK;
    applyTheme(next);
  }

  // Apply theme immediately on script load (before DOM ready)
  applyTheme(getPreferredTheme());

  // Bind click handler when DOM is ready
  function bindToggle() {
    var btn = document.querySelector('.blockr-theme-toggle');
    if (btn) {
      btn.addEventListener('click', toggleTheme);
      updateToggleButton(getPreferredTheme());
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', bindToggle);
  } else {
    bindToggle();
  }

  // Also listen for system preference changes
  if (window.matchMedia) {
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function(e) {
      if (!getStoredTheme()) {
        applyTheme(e.matches ? DARK : LIGHT);
      }
    });
  }
})();
