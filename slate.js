var leftHalf = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX/2",
    height: "screenSizeY"
});

var rightHalf = slate.operation("move", {
    x: "screenOriginX+screenSizeX/2",
    y: "screenOriginY",
    width: "screenSizeX/2",
    height: "screenSizeY"
});


var topHalf = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX",
    height: "screenSizeY/2"
});

var bottomHalf = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY+screenSizeY/2",
    width: "screenSizeX",
    height: "screenSizeY/2"
});

var full = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX",
    height: "screenSizeY"
});

var moveScreen0 = slate.operation("throw", {
    screen: "0"
});

var moveScreen1 = slate.operation("throw", {
    screen: "1"
});

var moveScreen2 = slate.operation("throw", {
    screen: "2"
});

var moveScreen3 = slate.operation("throw", {
    screen: "3"
});


slate.bind("down:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(bottomHalf);
});


slate.bind("up:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(topHalf);
});

slate.bind("left:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(leftHalf);
});

slate.bind("right:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(rightHalf);
});

slate.bind("left:ctrl,alt,cmd", function (win) {
    if (!win) {
        return;
    }

    var screen = win.screen().id() - 1;

    if(screen < 0){
	screen = 3
    }
    slate.log(screen);
    var op = slate.operation("throw", {
	screen: screen.toString()
    });
    win.doOperation(op);

});

slate.bind("right:ctrl,alt,cmd", function (win) {
    if (!win) {
        return;
    }

    var screen = win.screen().id() + 1;

    if(screen > 3){
	screen = 0
    }
    slate.log(screen);
    var op = slate.operation("throw", {
	screen: screen.toString()
    });
    win.doOperation(op);

});


slate.bind("f:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(full);
});

slate.bind("0:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(moveScreen0);
});


slate.config("orderScreensLeftToRight", true);





var screen0Top = slate.operation("move",{
    x: "screenOriginX-5",
    y: "screenOriginY",
    width: "screenSizeX-5",
    height: "screenSizeY/2",
    screen: "0"
});
var screen0Bottom =  slate.operation("move",{
    x: "screenOriginX-5",
    y: "screenOriginY+screenSizeY/2",
    width: "screenSizeX-5",
    height: "screenSizeY/2",
    screen: "0"
});
var fourMonitorLayout = slate.layout("fourMonitor", {
    "Slack": {
        "operations": [screen0Top],
        "main-first": true,
        "ignore-fail":true,
        "repeat" : true
    },
    "HipChat": {
        "operations": [screen0Top],
        "main-first": true,
        "ignore-fail":true,
        "repeat" : true
    },
    "Microsoft Outlook": {
        "operations":[screen0Bottom],
        "main-first": true,
        "ignore-fail":true,
        "repeat": true        
    },
    "VMware Fusion": {
        "operations": [moveScreen1, full],
        "main-first": true,
        "ignore-fail":true,
        "repeat" : true
    }

});

slate.bind("a:ctrl,alt,cmd", slate.operation("layout", {
    name: fourMonitorLayout
}));



slate.default(6, fourMonitorLayout);

if(slate.screenCount() == 4){
  slate.operation("layout", {
    name: fourMonitorLayout
  }).run()  
}

slate.log("screen count " + slate.screenCount());
slate.eachScreen(function(screenObject) {
    slate.log(screenObject.id()+" "+JSON.stringify(screenObject.rect()));
});
