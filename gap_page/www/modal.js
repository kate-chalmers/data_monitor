/* $(document).ready(function() {
  $(document).on('click', '.card-front', function() {
    var classList = Array.from(this.classList); // avoids regex escaping
    var anyUnderscore = classList.find(c => c.includes('_')) || null;
    Shiny.setInputValue('clicked_class', anyUnderscore, {priority: 'event'});

    // NEW: whitelist the dimension classes
    var dimClasses = ['gender_dim','age_dim','educ_dim'];
    var measureClass = classList.find(c => dimClasses.includes(c)) || null;
    var measure = measureClass ? measureClass.replace(/_dim$/, '') : null;

    Shiny.setInputValue('clicked_class2', measureClass, {priority: 'event'});
    Shiny.setInputValue('clicked_dim', measure, {priority: 'event'});

    $('#modal1').modal('show');
  });
});
*/

$(document).ready(function() {
  $(document).on('click', '.card-front', function() {
    var classList = $(this).attr('class').split(' ');
    var measureClass = classList.find(c => c.includes('_'));
    Shiny.setInputValue('clicked_class', measureClass, {priority: 'event'});
    $('#modal1').modal('show');
  });
});
