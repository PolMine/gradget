var camera, controls, scene, raycaster, renderer, controls;

window.mouseX = 100;
window.mouseY = 100;

var INTERSECTED, MATCH;
var radius = 100, theta = 0;

var mouse = {x: 100, y: 100};

var rect = document.getElementById("graph").getBoundingClientRect();


function init(){
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
  
  document.getElementById('content').appendChild( container );
  
  renderer = new THREE.WebGLRenderer();
  renderer.setSize(window.innerWidth, window.innerHeight)
  renderer.setClearColor(0xcccccc, 1);
  // document.body.appendChild(renderer.domElement);
  container.appendChild( renderer.domElement );
  window.addEventListener( 'resize', onWindowResize, false );
  
  
  raycaster = new THREE.Raycaster();
  container.addEventListener( 'mousemove', firstMouseMove, false );
}
            
function onWindowResize() {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize( window.innerWidth, window.innerHeight );
  var rect = document.getElementById("graph").getBoundingClientRect();
  window.screen.availWidth;
  window.screen.availHeight;
  screen.width;
  screen.height;
  // console.log(rect.top, rect.right, rect.bottom, rect.left);
  // console.log(window.innerHeight)

}

function firstMouseMove ( event ) {
	
	event.preventDefault();
	
	window.mouseX = ( event.clientX / window.innerWidth ) * 2 - 1;
	window.mouseY = - ( event.clientY / window.innerHeight ) * 2 + 1;
  
  mouse.x = event.clientX;
  mouse.y = event.clientY;
  
  render();
  
}


function animate(){
    requestAnimationFrame( animate );
    controls.update();
}


function render(){

		camera.lookAt( scene.position );
    scene.getObjectByName( "pointLight" ).position.copy( camera.position );
		
		// explanation: where does 0.4 come from ? 195 / 975 * 2, assuming width of 1170 and a grid consisting of 12 rows;
		
			// event.preventDefault();

		// var mouseX = ( window.clientX / window.innerWidth ) * 2 - 1;
		// var	mouseY = - ( window.clientY / window.innerHeight ) * 2 + 1;

		
		var rect = document.getElementById("graph").getBoundingClientRect();
		
		// coordX identical with mouse.x if window is maximized
		var coordX = window.mouseX *  (screen.width / 2) + (screen.width / 2);
		var centerX = rect.left + rect.width / 2;
		var posX = (coordX - centerX) / centerX * 2;

		// var coordY = window.innerHeight / 2 - window.mouseY * (window.innerHeight / 2);
		
		var coordY = (- window.mouseY + 1) / 2 * window.innerHeight;
		var posY = (((rect.bottom - rect.top) / 2) - (mouse.y - rect.top)) / ((rect.bottom - rect.top) / 2);
		
		console.log(posY);
		// console.log("mouse.x ", mouse.x);
		// console.log(window.innerHeight);
		var vector = new THREE.Vector3(
		  window.mouseX - calibrationX,
		  window.mouseY - calibrationY, 1
		  ).unproject( camera );
		// var vector = new THREE.Vector3( posX , coordY, 1 ).unproject( camera );
		raycaster.set( camera.position, vector.sub( camera.position ).normalize() );
		var intersects = raycaster.intersectObjects( scene.children );
		
		if ( intersects.length > 0 ) {
			for (var i = 0; i < intersects.length; i++){
        if (intersects[i].object instanceof THREE.Sprite == false){
          if ( intersects[i].object instanceof THREE.Line ) {
            j = eval(intersects[i].object.id) - 5 - text.length;
            console.log("edge no: " + j);
            console.log(Object.keys(edgeData));
		      } else if ( intersects[i].object instanceof THREE.Mesh ) {
            j = eval(intersects[i].object.id) - 5;
            var vertexInfo = text[j + 1];
            var vertexDataColumns = Object.keys(vertexData);
            console.log(vertexInfo);
            Shiny.onInputChange('node', vertexInfo);
            console.log(vertexDataColumns);
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
    renderer.render( scene, camera );
}

// init();
// animate();
