var camera, controls, scene, raycaster, renderer, controls;

window.mouseX = 100;
window.mouseY = 100;

var INTERSECTED, MATCH;
var radius = 100, theta = 0;

var spacehits = 0;

// defined for anaglyph effect
var anaglyphMode = true;
var width = window.innerWidth || 2;
var height = window.innerHeight || 2;

var calibrationX = 5;
Shiny.addCustomMessageHandler("passCallibrationX", function(calibrateX) {calibrationX = calibrateX;});

var calibrationY = 5;
Shiny.addCustomMessageHandler("passCallibrationY", function(calibrateY) {calibrationY = calibrateY;});


function createScene(){
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
  return scene;
  
}

function createGraph(scene){
  
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
	
	return scene;

}

function createContainer(){
  var container = document.createElement( 'div' );
  container.setAttribute("id", "graph")
  
  renderer = new THREE.WebGLRenderer();
  renderer.setSize(window.innerWidth, window.innerHeight)
  renderer.setClearColor(0xcccccc, 1);
  
  container.appendChild( renderer.domElement );
  
  return container;
}

function init(){
  
  scene = createScene();
  scene = createGraph(scene);
  container = createContainer();

  window.addEventListener( 'resize', onWindowResize, false );
  window.addEventListener( 'keydown', onKeyboardInput, true );
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
            
function onWindowResize() {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize( window.innerWidth, window.innerHeight );
}

function onKeyboardInput( event ){
  if (event.defaultPrevented) {
    return; // Do nothing if the event was already processed
  }
  if (event.keyCode === 32){
    spacehits ++;
    Shiny.onInputChange('graph_space_pressed', spacehits);
  }
  event.preventDefault();
}

function firstMouseMove ( event ) {
	
	event.preventDefault();
	
	window.mouseX = ( event.clientX / window.innerWidth ) * 2 - 1;
	window.mouseY = - ( event.clientY / window.innerHeight ) * 2 + 1;
  
  render();
  
}


function animate(){
    requestAnimationFrame( animate );
    controls.update();
}


function render(){

		camera.lookAt( scene.position );
    scene.getObjectByName( "pointLight" ).position.copy( camera.position );
		
		var vector = new THREE.Vector3(
		  window.mouseX - calibrationX,
		  window.mouseY - calibrationY, 1
		  ).unproject( camera );
		  
		raycaster.set( camera.position, vector.sub( camera.position ).normalize() );
		var intersects = raycaster.intersectObjects( scene.children );
		
		if ( intersects.length > 0 ) {
			for (var i = 0; i < intersects.length; i++){
        if (intersects[i].object instanceof THREE.Sprite == false){
          if ( intersects[i].object instanceof THREE.Line ) {
            
            j = eval(intersects[i].object.id) - 4 - sphereCoords.x.length;
            Shiny.onInputChange('graph_edge_selected_a', edgeData["a"][j]);
            Shiny.onInputChange('graph_edge_selected_b', edgeData["b"][j]);

		      } else if ( intersects[i].object instanceof THREE.Mesh ) {
            
            j = eval(intersects[i].object.id) - 5;
            var vertexInfo = text[j + 1];
            Shiny.onInputChange('graph_node_selected', vertexInfo);
            
		      }
          MATCH = intersects[i].object
          break
  			}
  		}
      
      if ( INTERSECTED != MATCH ) {
				if ( INTERSECTED ) {
				  if ( INTERSECTED instanceof THREE.Line ) {
            INTERSECTED.material.setValues( {color: eval(linesColor) });
  		    }            
            if ( INTERSECTED instanceof THREE.Mesh ) {
            INTERSECTED.material.emissive.setHex( INTERSECTED.currentHex );
          }              
				}
				INTERSECTED = MATCH;            
        if ( INTERSECTED instanceof THREE.Line ) {
           var currentHex = INTERSECTED.material.color.getHexString();
           INTERSECTED.material.setValues({color: 0xff0000})
        } else if ( INTERSECTED instanceof THREE.Mesh ) {
         INTERSECTED.currentHex = INTERSECTED.material.emissive.getHex();
         INTERSECTED.material.emissive.setHex( 0xff0000 );
        }
			}
		} else {
			if ( INTERSECTED ) {
        if ( INTERSECTED instanceof THREE.Line ) {
          INTERSECTED.material.setValues( {color: eval(linesColor) });
			  }            
        if ( INTERSECTED instanceof THREE.Mesh ) {
          INTERSECTED.material.emissive.setHex( INTERSECTED.currentHex );
        }
			}
			INTERSECTED = null;
		}

    scene.getObjectByName( "pointLight" ).position.copy( camera.position );
    if (anaglyphMode == "true"){
      effect.render( scene, camera );
    } else {
      renderer.render( scene, camera );
    }
}
