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


var topThird = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX",
    height: "screenSizeY/3"
});

var middleThird = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY+screenSizeY/3",
    width: "screenSizeX",
    height: "screenSizeY/3"
});
var bottomThird = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY+(screenSizeY*(2/3))",
    width: "screenSizeX",
    height: "screenSizeY/3"
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

var fullDubble = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX*2",
    height: "screenSizeY"
});

var topLeft = S.op("corner", {
    "direction": "top-left",
    "width": "screenSizeX/2",
    "height": "screenSizeY/2"
});
var topLeftShort = S.op("corner", {
    "direction": "top-left",
    "width": "screenSizeX/2",
    "height": "screenSizeY*0.40"
});

var topRight = S.op("corner", {
    "direction": "top-right",
    "width": "screenSizeX/2",
    "height": "screenSizeY/2"
});
var bottomLeft = S.op("corner", {
    "direction": "bottom-left",
    "width": "screenSizeX/2",
    "height": "screenSizeY/2"
});
var bottomLeftTall = S.op("corner", {
    "direction": "bottom-left",
    "width": "screenSizeX/2",
    "height": "screenSizeY*0.6"
});

var bottomRight = S.op("corner", {
    "direction": "bottom-right",
    "width": "screenSizeX/2",
    "height": "screenSizeY/2"
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
var moveScreen4 = slate.operation("throw", {
    screen: "4"
});
var moveScreen5 = slate.operation("throw", {
    screen: "5"
});

function googleChromeLayout(windowObject) {
    var title = windowObject.title();
    slate.log(title);
    if (title !== undefined && title.match(/^Grafana.+$/)) {
        windowObject.doOperation(moveScreen0);
        windowObject.doOperation(full);
    }  else if (title !== undefined && title == "Postman") {
        //do nothing
    } else {
        windowObject.doOperation(moveScreen1);
        windowObject.doOperation(full);
    }
}


slate.config("orderScreensLeftToRight", true);

var screen0Top = slate.operation("move", {
    x: "screenOriginX-5",
    y: "screenOriginY",
    width: "screenSizeX-5",
    height: "screenSizeY/2",
    screen: "0"
});
var screen0Bottom = slate.operation("move", {
    x: "screenOriginX-5",
    y: "screenOriginY+screenSizeY/2",
    width: "screenSizeX-5",
    height: "screenSizeY/2",
    screen: "0"
});
var screen3Full = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX",
    height: "screenSizeY",
    screen: "3"
});
var screen4Full = slate.operation("move", {
    x: "screenOriginX",
    y: "screenOriginY",
    width: "screenSizeX",
    height: "screenSizeY",
    screen: "4"
});


var threeMonitorLayout = slate.layout("threeMonitor", {
    "Slack": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(topLeftShort);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Microsoft Outlook": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(bottomLeftTall);
        }],
        "main-first": true,
        "ignore-fail": false,
        "repeat": true
    },
    "Google Play Music Desktop Player": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(bottomRight);
        }],
        "main-first": true,
        "ignore-fail": false,
        "repeat": true
    },
    "VMware Fusion": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen1);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Google Chrome": {
        "operations": [googleChromeLayout],
        "repeat": true,
        "ignore-fail": true
    },
    "Firefox": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen1);
            wo.doOperation(full);
        }],
        "repeat": true,
        "ignore-fail": true
    },
    "PyCharm Community Edition": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(topHalf);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "WebStorm": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(topHalf);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Emacs": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(topHalf);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "iTerm2": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(bottomHalf);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    }
});

var fourMonitorLayout = slate.layout("fourMonitor", {
    "Slack": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(topLeftShort);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Microsoft Outlook": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(bottomLeftTall);
        }],
        "main-first": true,
        "ignore-fail": false,
        "repeat": true
    },
    "Google Play Music Desktop Player": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(bottomRight);
        }],
        "main-first": true,
        "ignore-fail": false,
        "repeat": true
    },
    "VMware Fusion": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen1);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Google Chrome": {
        "operations": [googleChromeLayout],
        "repeat": true,
        "ignore-fail": true
    },
    "Firefox": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen0);
            wo.doOperation(full);
        }],
        "repeat": true,
        "ignore-fail": true
    },
    "PyCharm Community Edition": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "WebStorm": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
     "Xcode": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
     },
     "Android Studio": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen2);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Emacs": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen3);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "iTerm2": {
        "operations": [function(wo) {
            wo.doOperation(moveScreen1);
            wo.doOperation(full);
        }],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    }
});


slate.bind("f:ctrl,cmd", fullDubble)

slate.bind("a:ctrl,alt,cmd", function(wo){

if (slate.screenCount() == 3) {
    slate.operation("layout", {
        name: threeMonitorLayout
    }).run();
}
if (slate.screenCount() == 4) {
    slate.operation("layout", {
        name: fourMonitorLayout
    }).run();
}

});



slate.default(3, threeMonitorLayout);

if (slate.screenCount() == 3) {
    slate.operation("layout", {
        name: threeMonitorLayout
    }).run();
}
if (slate.screenCount() == 4) {
    slate.operation("layout", {
        name: fourMonitorLayout
    }).run();
}

slate.log("screen count " + slate.screenCount());
slate.eachScreen(function(screenObject) {
    slate.log(screenObject.id() + " " + JSON.stringify(screenObject.rect()));
});
