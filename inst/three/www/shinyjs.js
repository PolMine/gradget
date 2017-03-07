shinyjs.reinitialize = function(){
  init()
}

shinyjs.reanimate = function(){
  animate()
}

shinyjs.reloadData = function(x){
  console.error("resetting values")
  eval(x[0])
}

shinyjs.removeEventListeners = function(){
  controls.removeEventListener('change', render);
  window.removeEventListener( 'resize', onWindowResize);
  container.removeEventListener( 'mousemove', firstMouseMove);
  window.removeEventListener( 'keydown', onKeyboardInput);

}

shinyjs.twoDimGraph = function(svg){
  
  var container = document.createElement( 'div' );
  container.setAttribute("id", "graph");
  
  var xmlDoc = new DOMParser().parseFromString(svg[0], "text/xml");
  container.appendChild(xmlDoc.documentElement);
  
  if (document.getElementById('content').hasChildNodes() == true){
    graphNode = document.getElementById('graph');
    parentNode = graphNode.parentNode;
    parentNode.replaceChild(container, graphNode)
  } else {
    document.getElementById('content').appendChild( container );
  }

}
