exports.panic = function () {
    console.error ('macro unrecognized');
    //throw "panic";
}

exports.triple = function (a,b,c) {
    console.error ();
    console.error (a);
    console.error (b);
    console.error (c);
    console.error ();
   return {a,b,c};
}
