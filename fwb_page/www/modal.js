
// Modal definition search function

$(document).ready(function() {
  $(document).on('click', '.card', function() {
    var classList = $(this).attr('class').split(' ');
    var measureClass = classList.find(c => c.includes('_'));
    Shiny.setInputValue('clicked_class', measureClass, {priority: 'event'});
    $('#modal1').modal('show');
  });
});