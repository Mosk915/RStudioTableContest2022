function(row, data, displayIndex) {
  for (n = 1; n <= 9; n++) {
    //If the value in the COLOR_USE column is TRUE, set the background color of the cell to the value 9 columns over to the right
    $('td:eq(' + n + ')', row).css('background-color', data[19] ? data[n + 9] : '');
  }
  for(n = 0; n <= 9; n++) {
    if(n == 0) {
      //For every cell in the first column, prevent anything from happening if the cell is clicked
      $('td:eq(' + n + ')', row).attr('onclick', 'event.stopPropagation();');
      //For every cell in the first column, add a left border
      $('td:eq(' + n + ')', row).css('border-left', '1px solid #E1E1E1');
    }
    if(displayIndex + n <= this.api().rows().count()) {
      //For every cell in the top left diagonal half of the table, add a bottom and right border
      $('td:eq(' + n + ')', row).css('border-bottom', '1px solid #E1E1E1');
      $('td:eq(' + n + ')', row).css('border-right', '1px solid #E1E1E1');
    } else {
      //For every cell in the bottom right diagonal half of the table, prevent anything from happening if the cell is clicked and make the background white
      $('td:eq(' + n + ')', row).attr('onclick', 'event.stopPropagation();');
      $('td:eq(' + n + ')', row).css('background-color', '#FFFFFF');
    }
  }
}
