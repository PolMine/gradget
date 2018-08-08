HTMLWidgets.widget({

  name: "three",
  
  type: "output",
  
  factory: function(el, width, height) {
  
    // create our sigma object and bind it to the element
    console.log(el);
    var container = document.createElement( 'div' );
    el.appendChild( container );

    return {
      renderValue: function(x) {
        
        // it is necessary to get rid of the padding because if not,
        // coordinates will be twisted and the raycaster will not work
        if (x.settings.raycaster == true){
          // document.getElementsByTagName("body")[0].style.padding = "0px";
          // document.getElementById("htmlwidget_container").style.top ="0px";
          // document.getElementById("htmlwidget_container").style.left ="0px";
        }
        
        // variables needed one way or the other
        var camera, controls, scene, renderer;

        // variables needed for raycaster
        if (x.settings.raycaster == true){
          var raycaster, INTERSECTED, MATCH;
          window.mouseX = 100; // just to start with any value?
          window.mouseY = 100; // just to start with any value?
        };

        function init(){
          
          camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 1, 1000);
          camera.position.z = 500;

          controls = new THREE.TrackballControls( camera );
          if (x.settings.raycaster == true){
            controls.addEventListener('change', render, false);
          } else {
            controls.addEventListener('change', render);
          };

          

          scene = new THREE.Scene();
                
          var pointLight = new THREE.PointLight( 0xffffff, 1, 1200 );
          pointLight.name = "pointLight";
          scene.add( pointLight );
          pointLight.position.set( camera.position.x, camera.position.y, camera.position.z + 10 );
          
          scene.add( new THREE.AmbientLight( 0x444444 ) );
          var particleLight;
          particleLight = new THREE.Mesh(
            new THREE.SphereGeometry( 4, 8, 8 ),
            new THREE.MeshBasicMaterial( { color: 0xffffff } ) 
            );
          scene.add( particleLight );
          var pointLight = new THREE.PointLight( 0xaaaaaa, 1);
          particleLight.add( pointLight );
          particleLight.position.x = camera.position.x;
          particleLight.position.y = camera.position.y;
          particleLight.position.z = camera.position.z + 20;

          
                
          var size = 256; // CHANGED
          
          for (var i = 0; i < x.data.edge_data.from.x.length; i++){
            var material = new THREE.LineBasicMaterial(
              { color: eval(x.data.edge_data.color), linewidth: eval(x.data.edge_data.lwd)}
            );
            var geometry = new THREE.Geometry();
            geometry.vertices.push(
              new THREE.Vector3(
                x.data.edge_data.from.x[i],
                x.data.edge_data.from.y[i],
                x.data.edge_data.from.z[i]
                ),
              new THREE.Vector3(
                x.data.edge_data.to.x[i],
                x.data.edge_data.to.y[i],
                x.data.edge_data.to.z[i] )
              );
            var line = new THREE.Line( geometry, material );
            scene.add(line);
          };
          
          for (var i = 0; i < x.data.point_data.x.length; i++) {
            var geometry = new THREE.SphereGeometry( x.data.point_data.nodeSize[i], 16, 16 );
            var material = new THREE.MeshLambertMaterial(
              {color: x.data.point_data.color[i], shading: THREE.FlatShading }
            );
            var mesh = new THREE.Mesh(geometry, material);
            mesh.position.set(x.data.point_data.x[i], x.data.point_data.y[i], x.data.point_data.z[i]);
            mesh.name = '' + i;
            scene.add(mesh);
          };
          
          for (var i = 0; i < x.data.text_data.name.length; i++){
            var canvas = document.createElement('canvas');
            canvas.width = size;
            canvas.height = size;
            var context = canvas.getContext('2d');
            context.fillStyle = x.data.text_data.fontColor[i];
            context.textAlign = 'center';
            context.font = x.data.text_data.fontSize + 'px Arial';
            context.fillText(x.data.text_data.name[i], size / 2, size / 2);
            var amap = new THREE.Texture(canvas);
            amap.needsUpdate = true;
            var mat = new THREE.SpriteMaterial({map: amap, transparent: false, useScreenCoordinates: false, color: 0xffffff});
            var sp = new THREE.Sprite(mat);
            sp.scale.set( 100, 100, 10 );
            sp.position.set(x.data.text_data.x[i] - 10, x.data.text_data.y[i] - 10, x.data.text_data.z[i] - 10);
            scene.add(sp);
          };

          // var container = document.createElement( 'div' );
          // document.appendChild( container );
          // document.body.appendChild( container );
          
          // this is a play to output information from raycaster
          
          if (x.settings.raycaster == true){
            var info = document.createElement( 'div' );
            info.style.position = 'absolute';
            info.style.top = '10px';
            info.style.width = '100%';
            info.style.textAlign = 'left';
            info.setAttribute("id", "info");
            info.innerHTML = "empty\nempty";
            container.appendChild( info );
          
            raycaster = new THREE.Raycaster();
          };


          renderer = new THREE.WebGLRenderer();
          renderer.setSize(window.innerWidth, window.innerHeight)
          renderer.setClearColor(eval(x.settings.bgColor), 1);
          // renderer.sortObjects = false; // from raycaster.html
          container.appendChild( renderer.domElement );
          
          window.addEventListener( 'resize', onWindowResize, false );
          if (x.settings.raycaster == true){
            window.addEventListener( 'mousemove', firstMouseMove, false ); 
          };
          // render()
          
        }
            
        function onWindowResize() {
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize( window.innerWidth, window.innerHeight );
        }
        
        function firstMouseMove ( event ) {
          // is activated when mouse moves into window
          event.preventDefault();
          window.mouseX = ( event.clientX / window.innerWidth ) * 2 - 1;
          window.mouseY = - ( event.clientY / window.innerHeight ) * 2 + 1;
          document.getElementById("info").innerHTML = "" + camera.position.x;
          render()
        }

        function animate(){
          requestAnimationFrame( animate );
          controls.update();
        }

        function render(){
          scene.getObjectByName( "pointLight" ).position.copy( camera.position );
          
          if (x.settings.raycaster == true){
            camera.lookAt( scene.position );
            var vector = new THREE.Vector3( window.mouseX, window.mouseY, 1 ).unproject( camera );
            raycaster.set( camera.position, vector.sub( camera.position ).normalize() );
            var intersects = raycaster.intersectObjects( scene.children );
  
  				  if ( intersects.length > 0 ) {
  
  					  for (var i = 0; i < intersects.length; i++){
          			
                if (intersects[i].object instanceof THREE.Sprite == false){
  
                  if ( intersects[i].object instanceof THREE.Line ) {
   
                    j = eval(intersects[i].object.id) - 5 - 0;
                    var edgeInfo = "edge no: " + j;
                    // var edgeDataColumns = Object.keys(edgeData);
                    // for (var k = 0; k < edgeDataColumns.length; k++){
                    //   edgeInfo = edgeInfo + "<br/>" + edgeDataColumns[k] + ": " + edgeData[edgeDataColumns[k]][j];
                    // }
                    document.getElementById("info").innerHTML = edgeInfo;
  
      			      } else if ( intersects[i].object instanceof THREE.Mesh ) {
  
                    j = eval(intersects[i].object.id) - 5;
                    vertexInfo = 'This is:' + j;
                    document.getElementById("info").innerHTML = vertexInfo;
                  
                  
        		      }
                  MATCH = intersects[i].object
                  break
          			}
        		}
            
            if ( INTERSECTED != MATCH ) {
  
  						if ( INTERSECTED ) {
    					  if ( INTERSECTED instanceof THREE.Line ) {
                  INTERSECTED.material.setValues( {color: eval(x.data.edge_data.color) });
        		    } if ( INTERSECTED instanceof THREE.Mesh ) {
                  INTERSECTED.material.setValues( { color: INTERSECTED.currentHex });
                  // INTERSECTED.material.emissive.setHex( INTERSECTED.currentHex );
                }              
  						}
  
  						INTERSECTED = MATCH;            
              if ( INTERSECTED instanceof THREE.Line ) {
                 var currentHex = INTERSECTED.material.color.getHexString();
                 INTERSECTED.material.setValues({color: 0xff0000})
              } else if ( INTERSECTED instanceof THREE.Mesh ) {
               INTERSECTED.currentHex = INTERSECTED.material.color.getHex();
               // INTERSECTED.currentHex = INTERSECTED.material.emissive.getHex();
               INTERSECTED.material.setValues( { color: 0xff0000 });
               // INTERSECTED.material.emissive.setHex( 0xff0000 );
              }
  
  					}
  
  				} else {
  
  					if ( INTERSECTED ) {
              if ( INTERSECTED instanceof THREE.Line ) {
                INTERSECTED.material.setValues( {color: eval(x.data.edge_data.color) });
      			  }            
              if ( INTERSECTED instanceof THREE.Mesh ) {
                // INTERSECTED.material.emissive.setHex( INTERSECTED.currentHex );
                INTERSECTED.material.setValues( { color: INTERSECTED.currentHex });
              }
              
  					}
  
  					INTERSECTED = null;
  
  				}
  
            }

          
          renderer.render( scene, camera );
          
        } 
            
        init();
        animate();

      },
      
      resize: function(width, height) {
      }
    };
  }
});