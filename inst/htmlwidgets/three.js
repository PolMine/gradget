HTMLWidgets.widget({

  name: "three",
  
  type: "output",
  
  factory: function(el, width, height) {
  
    // create our sigma object and bind it to the element
    // var sig = new sigma(el.id);
    
    return {
      renderValue: function(x) {
        
        console.log(x);
        
        var camera, controls, scene, renderer;

        function init(){
          camera = new THREE.PerspectiveCamera(45, window.innerWidth / window.innerHeight, 1, 1000);
          camera.position.z = 500;

          controls = new THREE.TrackballControls( camera );
          controls.addEventListener('change', render);

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
            // console.log(x.data.text_data.fontColor[i]);
            console.log(x.data.text_data.fontColor[i]);
            context.fillStyle = x.data.text_data.fontColor[i];
            console.log(context.fillStyle);
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

          var container = document.createElement( 'div' );
          document.body.appendChild( container );
                
          renderer = new THREE.WebGLRenderer();
          renderer.setSize(window.innerWidth, window.innerHeight)
          renderer.setClearColor(eval(x.settings.bgColor), 1);
          // document.body.appendChild(renderer.domElement);
          container.appendChild( renderer.domElement );
          window.addEventListener( 'resize', onWindowResize, false );
        }
            
        function onWindowResize() {
          camera.aspect = window.innerWidth / window.innerHeight;
          camera.updateProjectionMatrix();
          renderer.setSize( window.innerWidth, window.innerHeight );
        }

        function animate(){
          requestAnimationFrame( animate );
          controls.update();
        }

        function render(){
          scene.getObjectByName( "pointLight" ).position.copy( camera.position );
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