function(row, data, displayIndex) {
  for(n = 0; n <= 10; n++) {
    if(n == 0) {
      //For every cell in the first column, add left and right borders
      $('td:eq(' + n + ')', row).css('border-left', '1px solid #E1E1E1');
      $('td:eq(' + n + ')', row).css('border-right', '1px solid #E1E1E1');
    }
    if(displayIndex + n == this.api().rows().count()) {
      //For every cell in the main diagonal running from the bottom right to the top left, add a bottom and right bold border
      $('td:eq(' + n + ')', row).css('border-bottom', '1px solid #000');
      $('td:eq(' + n + ')', row).css('border-bottom-width', 'medium');
      $('td:eq(' + n + ')', row).css('border-right', '1px solid #000');
      $('td:eq(' + n + ')', row).css('border-right-width', 'medium');
    }
    if(displayIndex + n > this.api().rows().count()) {
      //For every cell in the bottom right diagonal half of the table, make the text italicized and gray
      $('td:eq(' + n + ')', row).css('font-style', 'italic');
      $('td:eq(' + n + ')', row).css('color', '#777');
    }
  }
}
