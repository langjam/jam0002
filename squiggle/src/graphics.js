const fs = require("fs");

const ADDITIVE_MODE = "ADDITIVE";
const SUBTRACTIVE_MODE = "SUBTRACTIVE";
const MULT_MODE = "MULT";
const DIV_MODE = "DIV";
const MAX_VAL = 256;

const createCanvas = (width, height, toCopy) => {
    const canvas = new Array(height);
    for (let y = 0; y < canvas.length; y++) {
        canvas[y] = new Array(width);
        for (let x = 0; x < width; x++) {
            if (toCopy) {
                canvas[y][x] = toCopy[y][x]
            } else {
                canvas[y][x] = [0, 0, 0];
            }
        }
    }
    return canvas;
}

const zipWith = (fn, m1, m2) => {
    return m1.map((v, i) => fn(v, m2[i]));
}

const constraintVal = v => {
    let nv = v % MAX_VAL;
    if (nv < 0) {
        nv = MAX_VAL + nv;
    }
    return nv;
}

const addRGB = (rgb1, rgb2) => {
    return zipWith(((v1, v2) => constraintVal(v1 + v2)), rgb1, rgb2);
}

const subRGB = (rgb1, rgb2) => {
    return zipWith(((v1, v2) => constraintVal(v1 - v2)), rgb1, rgb2);
}

const multRGB = (rgb1, rgb2) => {
    return zipWith(((v1, v2) => constraintVal(v1 * v2)), rgb1, rgb2);
}

const divRGB = (rgb1, rgb2) => {
    return zipWith(((v1, v2) => constraintVal(v1 / (v2 == 0 ? 0.00001 : v2))), rgb1, rgb2);
}

const blendingModeToBlendFn = mode => ({
    [ADDITIVE_MODE]: addRGB,
    [SUBTRACTIVE_MODE]: subRGB,
    [MULT_MODE]: multRGB,
    [DIV_MODE]: divRGB
}[mode]);

const blendCanvases = (cs, mode) => {
    [fc, ...cs] = cs;
    const output = createCanvas(fc[0].length, fc.length, fc);
    const blendFn = blendingModeToBlendFn(mode);
    for (let y = 0; y < fc.length; y++) {
        for (let x = 0; x < fc[0].length; x++) {
            if (cs) {
                cs.forEach(c => {
                    output[y][x] = blendFn(output[y][x], c[y][x]);
                });
            }
        }
    }
    return output;
}

const toPPM = canvas => {
    const preambule = "P3\n" + canvas[0].length + " " + canvas.length + "\n" + MAX_VAL + "\n";
    let pixels = ""
    for (let y = 0; y < canvas.length; y++) {
        for (let x = 0; x < canvas[0].length; x++) {
            const [r, g, b] = canvas[y][x];
            pixels += Math.floor(r) + " " + Math.floor(g) + " " + Math.floor(b) + " ";
        }
        pixels += "\n";
    }
    fs.writeFileSync("output.ppm", preambule + pixels);
}

const setPixel = (canvas, x, y, rgb) => canvas[y][x] = rgb;

exports.toPPM = toPPM;
exports.blendCanvases = blendCanvases;
exports.createCanvas = createCanvas;
exports.addRGB = addRGB;
exports.subRGB = subRGB;
exports.setPixel = setPixel;
exports.constraintVal = constraintVal;
exports.constants = { ADDITIVE_MODE };