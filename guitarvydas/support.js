exports.panic = function () {
    console.error ('macro unrecognized');
    //throw "panic";
}

var param1stack = [];
var param2stack = [];

exports.pushParam1 = function (s) { param1stack.push (s); return ''; }
exports.pushParam2 = function (s) { param2stack.push (s); return ''; }

exports.getParam1 = function () { 
    var topx = param1stack.length - 1;
    return param1stack [topx];
}
exports.getParam2 = function () { 
    var topx = param2stack.length - 1;
    return param2stack [topx];
}

exports.popParams = function () {
    param1stack.pop ();
    param2stack.pop ();
}

