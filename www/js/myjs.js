$(function() {
  $(document).on({

    'shiny:busy': function(event) {
      $('.busyModal').modal('show');
    },
    'shiny:idle': function(event) {
      $('.busyModal').modal('hide');
    }
  })});