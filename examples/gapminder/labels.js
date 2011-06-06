function showLabel(evt, label) { 

  // Getting rid of any existing labels
  hideLabel();

  var svgNS = "http://www.w3.org/2000/svg";

  var target = evt.currentTarget;
  
  // Create new text node and text for the tooltip
  var content = document.createTextNode(label);  

  var text = document.createElementNS(svgNS, "text");
  text.setAttribute("id", "labelText");
  // Resetting some style attributes
  text.setAttribute("font-family", "sans-serif");
  text.setAttribute("font-size", "16px");
  text.setAttribute("fill", "black");
  text.setAttribute("stroke-width", "0");
  text.appendChild(content);

  // Add text to the bottom of the document.
  // This is because SVG has a rendering order.
  // We want the tooltip to be on top, therefore inserting last.
  var wrappingGroup = document.getElementsByTagName("g")[0];
  wrappingGroup.appendChild(text);

  // Transforming the mouse location to the SVG coordinate system
  // Snippet lifted from: http://tech.groups.yahoo.com/group/svg-developers/message/52701
  var m = target.getScreenCTM();
  var p = document.documentElement.createSVGPoint();
  p.x = evt.clientX;
  p.y = evt.clientY;
  p = p.matrixTransform(m.inverse());

  // Position tooltip rect and text
  text.setAttribute("transform", 
                    "translate(" + p.x + ", " + p.y + ") " +
                    "scale(1, -1)");
}

function hideLabel() {
  // Remove tooltip text 
  var text = document.getElementById("labelText");

  if (text !== null) {
    text.parentNode.removeChild(text);
  }
}

