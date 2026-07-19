// Fix: selectize event listener leak when Shiny re-renders mapping_ui.
// Selectize registers global scroll/mousedown/keydown listeners per instance,
// but never removes them when Shiny destroys the DOM. After many re-renders,
// hundreds of leaked listeners accumulate and block mouse-driven scrolling.

$(document).on('shiny:recalculating', function(e) {
  if (e.target && e.target.id === 'mapper-mapping_ui') {
    // Destroy selectize instances before Shiny tears out their DOM.
    // .destroy() unbinds all global scroll, mousedown, and keydown listeners.
    $(e.target).find('select.selectized').each(function() {
      if (this.selectize) {
        this.selectize.destroy();
      }
    });
  }
});

$(document).on('shiny:value', function(e) {
  if (e.name === 'mapper-mapping_ui') {
    // Remove any orphaned selectize dropdown divs left appended to <body>.
    // These appear when a dropdown was open at the moment of destruction
    // and can block mouse interaction until removed.
    $('body > .selectize-dropdown').remove();
  }
});
