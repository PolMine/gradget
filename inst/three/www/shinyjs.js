shinyjs.reinitialize = function(){
  
  console.error("reloading");

  camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 1, 1000);
  camera.position.z = 500;
  controls = new THREE.TrackballControls( camera );
  // controls.addEventListener('change', render);
  controls.addEventListener('change', render, false);
  scene = new THREE.Scene();
  var pointLight = new THREE.PointLight( 0xffffff, 1, 1200 );
  pointLight.name = "pointLight";
  scene.add( pointLight );
  pointLight.position.set( camera.position.x, camera.position.y, camera.position.z + 10 );
  
  var size = 256; // CHANGED
  var geometry = new THREE.SphereGeometry( sphereSize, 16, 16 );
  
  for (var i = 0; i < sphereCoords.x.length; i++) {
    var material = new THREE.MeshLambertMaterial( {color: eval(sphereColor[i]), shading: THREE.FlatShading });
    var mesh = new THREE.Mesh(geometry, material);
    mesh.position.set(sphereCoords.x[i], sphereCoords.y[i], sphereCoords.z[i]);
    mesh.name = ''+i;
    scene.add(mesh);
  }
  
  for (var i = 0; i < linesFrom.x.length; i++){
    var material = new THREE.LineBasicMaterial({ color: eval(linesColor), linewidth: eval(linesWidth) });
    var geometry = new THREE.Geometry();
    geometry.vertices.push(
      new THREE.Vector3( linesFrom.x[i], linesFrom.y[i], linesFrom.z[i] ),
      new THREE.Vector3( linesTo.x[i], linesTo.y[i], linesTo.z[i] )
    );
    var line = new THREE.Line( geometry, material );
    scene.add(line);
  }
  
  var size = 256;
  for (var i = 0; i < text.length; i++){
    var canvas = document.createElement('canvas');
    canvas.width = size;
    canvas.height = size;
    var context = canvas.getContext('2d');
    context.fillStyle = textColor;
    context.textAlign = 'center';
    context.font = textSize + 'px Arial';
    context.fillText(text[i], size / 2, size / 2);
    var amap = new THREE.Texture(canvas);
    amap.needsUpdate = true;
    var mat = new THREE.SpriteMaterial({map: amap, transparent: false, useScreenCoordinates: false, color: 0xffffff});
    var sp = new THREE.Sprite(mat);
    sp.scale.set( 100, 100, 10 );
    sp.position.set(textCoords.x[i] - textOffset.x, textCoords.y[i] - textOffset.y, textCoords.z[i] - textOffset.y);
    scene.add(sp);
  }
  
  var container = document.createElement( 'div' );
  container.setAttribute("id", "graph")
  
  
  renderer = new THREE.WebGLRenderer();
  renderer.setSize(window.innerWidth, window.innerHeight)
  renderer.setClearColor(0xcccccc, 1);
  // document.body.appendChild(renderer.domElement);
  
  container.appendChild( renderer.domElement );
  window.addEventListener( 'resize', onWindowResize, false );
  
  raycaster = new THREE.Raycaster();
  container.addEventListener( 'mousemove', firstMouseMove, false );
  
    // DIFFERENT
  graphNode = document.getElementById('graph');
  parentNode = graphNode.parentNode;
  parentNode.replaceChild(container, graphNode)
  // END difference

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
