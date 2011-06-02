var draggingElement = null;
var mousePressed = false;
var nMouseOffsetX = 0;
var nMouseOffsetY = 0;

function mouseDown(evt) {
    draggingElement = document.getElementById(eltName);
    mousePressed = true;

    var target = evt.currentTarget;
    draggingElement = target;
 
    if (target) {
        var p = document.documentElement.createSVGPoint();
        p.x = evt.clientX;
        p.y = evt.clientY;
        
        var m = target.getScreenCTM();
 
        p = p.matrixTransform(m.inverse());

        currentDragX = target.getAttribute("dragx");
        currentDragY = target.getAttribute("dragy");

        if (currentDragX !== null && currentDragY !== null) {
            nMouseOffsetX = p.x - parseInt(target.getAttribute("dragx"));
            nMouseOffsetY = p.y - parseInt(target.getAttribute("dragy"));
        }
        mousePressed = true;
    }
}

function mouseDown(evt) { 
    var target = evt.currentTarget;
    draggingElement = target;
 
    if (target) {
        var p = document.documentElement.createSVGPoint();
        p.x = evt.clientX;
        p.y = evt.clientY;
        
        var m = target.getScreenCTM();
 
        p = p.matrixTransform(m.inverse());

        currentDragX = target.getAttribute("dragx");
        currentDragY = target.getAttribute("dragy");

        if (currentDragX !== null && currentDragY !== null) {
            nMouseOffsetX = p.x - parseInt(target.getAttribute("dragx"));
            nMouseOffsetY = p.y - parseInt(target.getAttribute("dragy"));
        }
        mousePressed = true;
    }
}

function mouseUp(evt) { 
    draggingElement = null;
    mousePressed = false;
    nMouseOffsetX = 0;
    nMouseOffsetY = 0;
    alert(mousePressed);
}

function mouseMove(evt) { 
    if (mousePressed === true) {
        if (draggingElement !== null) {
            var p = document.documentElement.createSVGPoint();
            p.x = evt.clientX;
            p.y = evt.clientY;
    
            var m = evt.currentTarget.getScreenCTM();
     
            p = p.matrixTransform(m.inverse());
            p.x -= nMouseOffsetX;
            p.y -= nMouseOffsetY;
            
            if (draggingElement) {
                draggingElement.setAttribute("dragx", p.x);
                draggingElement.setAttribute("dragy", p.y);
                draggingElement.setAttribute("transform", "translate(" + p.x + "," + p.y + ")");
            }
        }
    }
}
