shinyjs.reinitialize = function(){
  
  scene = createScene();
  scene = createGraph(scene);
  container = createContainer();
  
  // window.addEventListener( 'resize', onWindowResize, false );
  // window.addEventListener( 'keydown', onKeyboardInput, true );
  container.addEventListener( 'mousemove', firstMouseMove, false );

  raycaster = new THREE.Raycaster();
  effect = new THREE.AnaglyphEffect( renderer ); // for anaglyph effect
  effect.setSize( width, height ); // for anaglyph effect

  if (document.getElementById('content').hasChildNodes() == true){
    console.log("replace");
    graphNode = document.getElementById('graph');
    parentNode = graphNode.parentNode;
    parentNode.replaceChild(container, graphNode)
  } else {
    console.log("first take")
    document.getElementById('content').appendChild( container );
  }
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
  console.error("removing content now")
  var container = document.createElement( 'div' );
  container.setAttribute("id", "graph");
  var xmlDoc = new DOMParser().parseFromString(svg[0], "text/xml");
  console.log(xmlDoc.documentElement)

  container.appendChild(xmlDoc.documentElement);
  graphNode = document.getElementById('graph');
  parentNode = graphNode.parentNode;
  parentNode.replaceChild(container, graphNode);
}
