// js functions to be included in svg dom element 

function edgeClick(x, y){
  
  document.getElementById('cooccurrences_a').value = x;
  Shiny.onInputChange('cooccurrences_a', x);
  
  document.getElementById('cooccurrences_b').value = y;
  Shiny.onInputChange('cooccurrences_b', y);
  
  document.getElementById('cooccurrences_any').value = Date().toString();
  Shiny.onInputChange('cooccurrences_any', Date().toString());
  
  spacehits ++;
  Shiny.onInputChange('graph_space_pressed', spacehits);
  
};

function nodeClick(x){
  
  document.getElementById('cooccurrences_a').value = x;
  Shiny.onInputChange('cooccurrences_a', x);
  
  document.getElementById('cooccurrences_b').value = '';
  Shiny.onInputChange('cooccurrences_b', '');
  
  spacehits ++;
  Shiny.onInputChange('graph_space_pressed', spacehits);
};
