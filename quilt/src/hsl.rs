use image::Rgba;

/// Color represented in HSL for Quilt
#[derive(Clone, Copy, Debug, Default)]
pub struct Hsl {
    /// Hue in 0-360 degree
    pub h: u16,
    /// Saturation from 0 to 100
    pub s: u8,
    /// Luminosity from 0 to 100
    pub l: u8,
}

impl From<Rgba<u8>> for Hsl {
    fn from(rgb: Rgba<u8>) -> Self {
        HslFloats::from_rgb(&rgb.0).into()
    }
}

impl From<HslFloats> for Hsl {
    fn from(hsl: HslFloats) -> Self {
        Hsl {
            h: hsl.h.round() as u16,
            s: (hsl.s * 100.0) as u8,
            l: (hsl.l * 100.0) as u8,
        }
    }
}

impl From<Hsl> for HslFloats {
    fn from(hslq: Hsl) -> Self {
        HslFloats {
            h: hslq.h as f64,
            s: hslq.s as f64 / 100.0,
            l: hslq.l as f64 / 100.0,
        }
    }
}

// Code below from https://github.com/killercup/hsl-rs/blob/b8a30e1/src/lib.rs
/// Color represented in HSL
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Default)]
struct HslFloats {
    /// Hue in 0-360 degree
    pub h: f64,
    /// Saturation in 0...1 (percent)
    pub s: f64,
    /// Luminosity in 0...1 (percent)
    pub l: f64,
}

impl HslFloats {
    /// Convert RGB pixel value to HSL
    ///
    /// Expects RGB pixel to be a slice of three `u8`s representing the red, green and blue values.
    ///
    /// Algorithm from [go-color] by Brandon Thomson <bt@brandonthomson.com>. (Iternally converts
    /// the pixel to RGB before converting it to HSL.)
    ///
    /// [go-color]: https://github.com/bthomson/go-color
    #[allow(clippy::float_cmp, clippy::many_single_char_names)]
    pub fn from_rgb(rgb: &[u8]) -> HslFloats {
        use std::cmp::{max, min};

        let mut h: f64;
        let s: f64;
        let l: f64;

        let (r, g, b) = (rgb[0], rgb[1], rgb[2]);

        let max = max(max(r, g), b);
        let min = min(min(r, g), b);

        // Normalized RGB: Divide everything by 255 to get percentages of colors.
        let (r, g, b) = (r as f64 / 255_f64, g as f64 / 255_f64, b as f64 / 255_f64);
        let (min, max) = (min as f64 / 255_f64, max as f64 / 255_f64);

        // Luminosity is the average of the max and min rgb color intensities.
        l = (max + min) / 2_f64;

        // Saturation
        let delta: f64 = max - min;
        if delta == 0_f64 {
            // it's gray
            return HslFloats {
                h: 0_f64,
                s: 0_f64,
                l,
            };
        }

        // it's not gray
        if l < 0.5_f64 {
            s = delta / (max + min);
        } else {
            s = delta / (2_f64 - max - min);
        }

        // Hue
        let r2 = (((max - r) / 6_f64) + (delta / 2_f64)) / delta;
        let g2 = (((max - g) / 6_f64) + (delta / 2_f64)) / delta;
        let b2 = (((max - b) / 6_f64) + (delta / 2_f64)) / delta;

        h = match max {
            x if x == r => b2 - g2,
            x if x == g => (1_f64 / 3_f64) + r2 - b2,
            _ => (2_f64 / 3_f64) + g2 - r2,
        };

        // Fix wraparounds
        if h < 0 as f64 {
            h += 1_f64;
        } else if h > 1_f64 {
            h -= 1_f64;
        }

        // Hue is precise to milli-degrees, e.g. `74.52deg`.
        let h_degrees = (h * 360_f64 * 100_f64).round() / 100_f64;

        HslFloats { h: h_degrees, s, l }
    }

    /// Convert HSL color to RGB
    #[allow(unused, clippy::many_single_char_names)]
    pub fn to_rgb(self) -> (u8, u8, u8) {
        if self.s == 0.0 {
            // Achromatic, i.e., grey.
            let l = percent_to_byte(self.l);
            return (l, l, l);
        }

        let h = self.h / 360.0; // treat this as 0..1 instead of degrees
        let s = self.s;
        let l = self.l;

        let q = if l < 0.5 {
            l * (1.0 + s)
        } else {
            l + s - (l * s)
        };
        let p = 2.0 * l - q;

        (
            percent_to_byte(hue_to_rgb(p, q, h + 1.0 / 3.0)),
            percent_to_byte(hue_to_rgb(p, q, h)),
            percent_to_byte(hue_to_rgb(p, q, h - 1.0 / 3.0)),
        )
    }
}

fn percent_to_byte(percent: f64) -> u8 {
    (percent * 255.0).round() as u8
}

/// Convert Hue to RGB Ratio
///
/// From <https://github.com/jariz/vibrant.js/> by Jari Zwarts
fn hue_to_rgb(p: f64, q: f64, t: f64) -> f64 {
    // Normalize
    let t = if t < 0.0 {
        t + 1.0
    } else if t > 1.0 {
        t - 1.0
    } else {
        t
    };

    if t < 1.0 / 6.0 {
        p + (q - p) * 6.0 * t
    } else if t < 1.0 / 2.0 {
        q
    } else if t < 2.0 / 3.0 {
        p + (q - p) * (2.0 / 3.0 - t) * 6.0
    } else {
        p
    }
}

#[cfg(test)]
mod test {
    use super::Hsl;
    use image::Rgba;

    #[test]
    fn test_hsl() {
        let rgba = Rgba([0x51, 0xff, 0x00, 0xff]);
        let hsl: Hsl = rgba.into();
        assert_eq!(hsl.h, 101);
    }
}
