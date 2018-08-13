window.popupX = 100;
window.popupY = 100;

var info = document.createElement( 'div' );
document.getElementById("htmlwidget_container").appendChild( info );
info.setAttribute("id", "info");
info.innerHTML = "no info so far";

info.style.position = 'absolute';
info.style.top = '10px';
info.style.left = '100px';
info.style.width = 'auto';
info.style.height = 'auto';
info.style.padding = "0.5em";
info.style.textAlign = 'left';
info.style.opacity = 0.65;
info.style.fontFamily = "arial";
info.style.fontSize = "12px";
info.style.borderRadius = "5px";
info.style.display = "none"; // to hide element at first
info.style.backgroundColor = "grey";
info.innerHTML = "some text";

function edgeMouseOver(el, me){
  console.log(el);
  info = document.getElementById("info");
  info.style.top = el.clientY + 10 + 'px';
  info.style.left = el.clientX + 'px';
  info.style.display = "block";
  var x = me.getAttribute("llxy")
  // var x = el.path[0].getNamedItem("llxy")
  console.log(x);
  info.innerHTML = x;
};

function nodeMouseOver(el){
  console.log("foo");
  info = document.getElementById("info");
  info.style.top = el.clientY + 10 + 'px';
  info.style.left = el.clientX + 'px';
  info.style.display = "block";
  info.innerHTML = "node";
};

