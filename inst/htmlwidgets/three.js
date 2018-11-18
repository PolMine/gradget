HTMLWidgets.widget({

  name: "three",
  
  type: "output",
  
  factory: function(el, width, height) {
  
    // create our sigma object and bind it to the element
    var container = document.createElement( 'div' );
    el.appendChild( container );
    
    // make width and height available globally, so that they can be retrieved/set in functions
    window.width = width;
    window.height = height;
    window.objectMatched;
    window.x;
    
    return {
      renderValue: function(x) {
        
        window.x = x; // for debugging
        
        // this is a somewhat dirty hack because for browser output
        // a default height of 400px is passed; here, we fill the whole
        // space. May conflict with knitr output.
        document.getElementById("three").style.height = "100%";
        if (x.settings.knitr == false){
          window.height = document.getElementById("three").offsetHeight;
        } else {
          if ( x.settings.width != null){
            console.log("a");
            console.log(x.settings.width);
            window.width = x.settings.width;
          }
          
          if ( x.settings.height != null){
            console.log("c");
            console.log(x.settings.height);
            window.height = x.settings.height;
          }
        }

        
        // variables needed one way or the other
        var camera, controls, scene, renderer;
        
        window.noAnnotations = 0;
        window.annotationObject = {};
        window.annotation = {};

        // variables needed for raycaster
        if (x.settings.raycaster == true){
          var raycaster, INTERSECTED, MATCH;
          window.mouseX;
          window.mouseY;
          window.popupX = 100;
          window.popupY = 100;
        };
        


        function init(){
          
          camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 1, 1000);
          camera.position.z = 500;

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
          
          for (var i = 0; i < x.data.vertex_data.x.length; i++) {
            var geometry = new THREE.SphereGeometry( x.data.vertex_data.nodeSize[i], 16, 16 );
            var material = new THREE.MeshLambertMaterial(
              {color: x.data.vertex_data.color[i], shading: THREE.FlatShading }
            );
            var mesh = new THREE.Mesh(geometry, material);
            mesh.position.set(x.data.vertex_data.x[i], x.data.vertex_data.y[i], x.data.vertex_data.z[i]);
            mesh.name = '' + i;
            scene.add(mesh);
          };
          
          for (var i = 0; i < x.data.vertex_data.name.length; i++){
            var canvas = document.createElement('canvas');
            canvas.width = size;
            canvas.height = size;
            var context = canvas.getContext('2d');
            context.fillStyle = x.data.vertex_data.fontColor[i];
            context.textAlign = 'center';
            context.font = x.data.vertex_data.fontSize + 'px Arial';
            context.fillText(x.data.vertex_data.name[i], size / 2, size / 2);
            var amap = new THREE.Texture(canvas);
            amap.needsUpdate = true;
            var mat = new THREE.SpriteMaterial({map: amap, transparent: false, useScreenCoordinates: false, color: 0xffffff});
            var sp = new THREE.Sprite(mat);
            sp.scale.set( 100, 100, 10 );
            sp.position.set(x.data.vertex_data.x[i] - 10, x.data.vertex_data.y[i] - 10, x.data.vertex_data.z[i] - 10);
            scene.add(sp);
          };


          // this is a play to output information from raycaster
          
          if (x.settings.raycaster == true){
            raycaster = new THREE.Raycaster();
          };
          
          if (x.settings.raycaster == true){
            var info = document.createElement( 'div' );
            container.appendChild( info );
            info.setAttribute("id", "info");
            info.innerHTML = "no info so far";

            info.style.position = 'absolute';
            info.style.top = '10px';
            info.style.left = '100px';
            // info.style.width = '80px';
            info.style.width = 'auto';
            info.style.height = 'auto';
            info.style.padding = "0.5em";
            info.style.textAlign = 'left';
            info.style.opacity = 0.65;
            info.style.fontFamily = "arial";
            info.style.fontSize = "12px";
            info.style.borderRadius = "5px";
            info.style.display = "none"; // to hide element at first
            info.style.backgroundColor = "white";
          };


          renderer = new THREE.WebGLRenderer();
          // renderer.setSize(window.innerWidth, window.innerHeight);
          renderer.setSize(window.width, window.height);
          renderer.setClearColor(eval(x.settings.bgColor), 1);
          // renderer.sortObjects = false; // from raycaster.html
          container.appendChild( renderer.domElement );
          
          if (x.settings.anaglyph == true){
            var width3D = window.innerWidth || 2;
            var height3D = window.innerHeight || 2;

            effect = new THREE.AnaglyphEffect( renderer ); // for anaglyph effect
            effect.setSize( width3D, height3D ); // for anaglyph effect
          };

          if (x.settings.knitr == true){
            document.getElementsByTagName("canvas")[0].style.borderRadius = "4px";
          }
          
          controls = new THREE.TrackballControls( camera, renderer.domElement );
          if (x.settings.raycaster == true){
            controls.addEventListener('change', render, false);
          } else {
            controls.addEventListener('change', render);
          };


          
          window.addEventListener( 'resize', onWindowResize, false );
          if (x.settings.raycaster == true){
            window.addEventListener( 'mousemove', firstMouseMove, false ); 
            window.addEventListener( 'contextmenu', addAnnotation, false );
          };
          // render()
          
        }
            
        function onWindowResize() {
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize( window.width, window.height );
        }
        
        function firstMouseMove ( event ) {
          // is activated when mouse moves into window
          event.preventDefault();
          window.mouseX = ( event.clientX / window.innerWidth ) * 2 - 1;
          window.mouseY = - ( event.clientY / window.innerHeight ) * 2 + 1;
          if (x.settings.raycaster == true){
            window.popupX = event.clientX;
            window.popupY = event.clientY;
          };
          render()
        }
        
        function getUserAnnotation(){
           var anno = bootbox.prompt({
             title: 'Annotate Edge/Node<hr style="padding: 0em;"/>\
              count: ' + x.data[window.objectMatched.type].count[window.objectMatched.index] + '<hr/>\
              <div style="height: 200px; font-weight: normal;font-size: smaller;overflow-y: scroll;">' + x.data[window.objectMatched.type].kwic[window.objectMatched.index] + '</div>\
              <hr/>\
              <div id="selection" class="btn-group" data-toggle="buttons">\
                <label class="radio-inline"><input type="radio" name="optradio" checked value="1">keep</label>\
                <label class="radio-inline"><input type="radio" name="optradio" value="2">reconsider</label>\
                <label class="radio-inline"><input type="radio" name="optradio" value="3">drop</label>\
              </div>',
             inputType: 'textarea',
            callback: function (result) {
              var selection = $('#selection input:radio:checked').val();
              window.annotation = {
                "selection": selection,
                "annotation": result,
                "type": window.annotationObject.type,
                "name": window.annotationObject.name
              };
              console.log(window.annotation);
              if (typeof Shiny != "undefined") {
                Shiny.onInputChange("annotation", window.annotation);
                Shiny.onInputChange('annotation_added', window.noAnnotations);
              }
              return window.annotation;
            }
           });
        };

        function addAnnotation( event ){
          // if (event.defaultPrevented) {
          //  return; // Do nothing if the event was already processed
          // }
          // if (event.keyCode === 32){
            window.noAnnotations ++;
            if (window.objectMatched.type == "vertex_data"){
              window.annotationObject = {"type":"vertex", "name": x.data.vertex_data.name[window.objectMatched.index]};
            } else if (window.objectMatched.type == "edge_data"){
              window.annotationObject = {"type":"edge", "name": x.data.edge_data.names[window.objectMatched.index]};
            };
            getUserAnnotation()
          // }
          event.preventDefault();
          
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
                    info.style.display = "block";
                    info.style.top = window.popupY + 10 + 'px';
                    info.style.left = window.popupX + 'px';
                    j = eval(intersects[i].object.id) - 7;
                    var edgeInfo = x.data.edge_data.names[j] + '<br/>ll: ' + x.data.edge_data.ll[i] + '<br/>count:  ' + x.data.edge_data.count[j];
                    info.innerHTML = edgeInfo;
                    window.objectMatched = {type: "edge_data", index: j};

  
      			      } else if ( intersects[i].object instanceof THREE.Mesh ) {
      			        info.style.display = "block";
                    info.style.top = window.popupY + 10 + 'px';
                    info.style.left = window.popupX + 'px';

                    j = eval(intersects[i].object.id) - 7 - x.data.edge_data.from.x.length;
                    vertexInfo = x.data.vertex_data.name[j] + '<br/>count: ' + x.data.vertex_data.count[j];
                    info.innerHTML = vertexInfo;
                    window.objectMatched = {type: "vertex_data", index: j};

        		      }
                  MATCH = intersects[i].object
                  break
          			}
        		}
            
            if ( INTERSECTED != MATCH ) {
  						if ( INTERSECTED ) {
    					  if ( INTERSECTED instanceof THREE.Line ) {
                  info.style.display = "none";
                  INTERSECTED.material.setValues( {color: eval(x.data.edge_data.color) });
        		    } if ( INTERSECTED instanceof THREE.Mesh ) {
                  info.style.display = "none";
                  INTERSECTED.material.setValues( { color: INTERSECTED.currentHex });
                  // INTERSECTED.material.emissive.setHex( INTERSECTED.currentHex );
                }              
  						}
  
  						INTERSECTED = MATCH;            
              
              if ( INTERSECTED instanceof THREE.Line ) {
                info.style.display = "none";
                var currentHex = INTERSECTED.material.color.getHexString();
                INTERSECTED.material.setValues({color: 0xff0000})
              } else if ( INTERSECTED instanceof THREE.Mesh ) {
                info.style.display = "none";
                INTERSECTED.currentHex = INTERSECTED.material.color.getHex();
                // INTERSECTED.currentHex = INTERSECTED.material.emissive.getHex();
                INTERSECTED.material.setValues( { color: 0xff0000 });
                // INTERSECTED.material.emissive.setHex( 0xff0000 );
              }
  
  					}
  
  				} else {
  				  
  				  info.style.display = "none";
  
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

          
          if (x.settings.anaglyph == true){
            effect.render( scene, camera );
          } else {
            renderer.render( scene, camera );
          }

        } 
            
        init();
        animate();

      },
      
      resize: function(width, height) {
      }
    };
  }
});
