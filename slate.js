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
    y: "screenOriginY+screenSizeY*0.66666",
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
        windowObject.doOperation(moveScreen1);
        windowObject.doOperation(full);
    }else if (title !== undefined && title.match(/Google Play Music.+$/)) {
        windowObject.doOperation(moveScreen0);
        windowObject.doOperation(middleThird);
    } else if (title !== undefined && title == "Postman") {
        //do nothing
    } else {
        windowObject.doOperation(moveScreen1);
        windowObject.doOperation(full);
    }
}

slate.bind("down:cmd,alt", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(bottomHalf);
});


slate.bind("up:cmd,ctrl", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(topThird);
});
slate.bind("down:cmd,ctrl", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(bottomThird);
});

slate.bind("m:cmd,ctrl", function (win) {
    if (!win) {
        return;
    }
    win.doOperation(middleThird);
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

    if (screen < 0) {
        screen = 5;
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

    if (screen > 5) {
        screen = 0;
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


var threeMonitorLayout= slate.layout("threeMonitor", {
    "Slack": {
        "operations": [moveScreen0, topThird],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Microsoft Outlook": {
        "operations": [moveScreen0, bottomThird],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "VMware Fusion": {
        "operations": [moveScreen1, full],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "Google Chrome": {
        "operations": [googleChromeLayout],
        "repeat": true,
        "ignore-fail": true
    },
    "PyCharm": {
        "operations": [moveScreen2, full],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    },
    "WebStorm": {
        "operations": [moveScreen2, full],
        "main-first": true,
        "ignore-fail": true,
        "repeat": true
    }
});


slate.bind("a:ctrl,alt,cmd", slate.operation("layout", {
    name: threeMonitorLayout
}));



slate.default(3, threeMonitorLayout);

if (slate.screenCount() == 3) {
    slate.operation("layout", {
        name: threeMonitorLayout
    }).run();
}

slate.log("screen count " + slate.screenCount());
slate.eachScreen(function (screenObject) {
    slate.log(screenObject.id() + " " + JSON.stringify(screenObject.rect()));
});
