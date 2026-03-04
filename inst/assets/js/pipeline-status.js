$(function () {
  // Handle chip click -> select block panel
  $(document).on('click', '.blockr-pipeline-chip', function () {
    var blockId = $(this).data('block-id');
    if (blockId) {
      Shiny.setInputValue('pipeline_chip_click', {
        id: blockId,
        nonce: Math.random()
      });
    }
  });

  // Handle overflow detection and fade edges
  function checkPipelineOverflow() {
    $('.blockr-pipeline-bar').each(function () {
      var bar = $(this);
      var scroll = bar.find('.blockr-pipeline-scroll');
      if (scroll.length && scroll[0].scrollWidth > scroll[0].clientWidth) {
        bar.addClass('has-overflow');
      } else {
        bar.removeClass('has-overflow');
      }
    });
  }

  // Check overflow on window resize and after Shiny renders
  $(window).on('resize', checkPipelineOverflow);
  $(document).on('shiny:value', function () {
    setTimeout(checkPipelineOverflow, 100);
  });
});
