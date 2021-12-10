# TEMPL User Guide
### Overview
TEMPL is a CSS-like language for getting visual patterns drawn on a virtual canvas. The syntax will also feel natural to those who have used JSON. It works on GNU/Linux, Windows and FreeBSD.

TEMPL was developed by [skejeton](https://github.com/skejeton) and [marekmaskarinec](https://github.com/marekmaskarinec) with some miscellaneous assistance from [RealDoigt](https://github.com/RealDoigt) and [jacobsebek](https://github.com/jacobsebek) .
### Installing
Clone this repository then type this in the terminal:
```sh
cd creators_of_templ
make
```
### Running Scripts
To run a TEMPL script, you have to open a terminal and type: `path\to\TEMPL path\to\script`. TEMPL source files have either the _.css_ extension. If everything is going smoothly, you should see a window with a white background appear. But we should not keep it white, let's get some drawing done!
### Drawing Simple Shapes
##### Program Structure
In TEMPL, you can consider your script to have three distinct areas; the top contains all the script's constants, the middle is where you put the script classes and the bottom is where the script starts. For now, we will not concern ourselves with constants and classes and focus only on the most important part.

if you want something to appear on the screen, you'll have to put it inside a `root` element. The `root` element can be understood as the entry point of the program from which the interpreter will be able to know what needs to be drown on the screen. A program doesn't need constants and classes, but it must have a `root` element to work correctly. Here's how it looks like:
 ```css
 root
 {

 }
 ```
 This element is where you will put all your other elements. Alone, `root` won't do much; it can't have any properties. Which is where the three other elements of the language come into play...
 ##### Rect, Circle and Triangle
 To draw something on the screen, you need to use one of the three basic shapes available: rectangles, circles and triangles.

Let's start with rectangles first. A Rectangle is known as `rect` in the language and it has 4 properties; position, dimensions, color and rotation. You're not forced to use all it's properties, but be aware that their default values are equal to 0, so that means an empty rectangle like this:
```css
 root
 {
	 rect
	 {

	 }
}
```
Will be black, have no rotation, be located at indices (0, 0) and will not show up on the canvas because it will have a size of 0 by 0. So let's add dimensions first so that we can at least see what it looks like;
```css
 root
 {
	 rect
	 {
		 dimensions: vec2(50, 50);
	 }
}
```
The above will result in a black square on the screen:
![black square](https://i.imgur.com/gjxvo75.png)

Two things to note: each property is terminated with a semi-colon and this square looks pretty lonely and bland, so let's add some more stuff! Next thing we should consider is adding some colour. Colour can written using two ways: by using the hexadecimal notation or by calling rgb. The two methods are equivalent, however hexadecimal is faster to write if you know what you're doing and rgb is more legible and has the ability to generate colours with varying degrees of opacity while colours in hexadecimal forms are always completely opaque. First, we'll use the hexadecimal notation to have a nice dark green square:
```css
 root
 {
	 rect
	 {
		 dimensions: vec2(50, 50);
		 color: #005000;
	 }
}
```
![dark green square](https://i.imgur.com/xrcKEVm.png)To compare the two usages, we'll add a blue companion to its right this time:
```css
 root
 {
	 rect
	 {
		 dimensions: vec2(50, 50);
		 color: #005000;
		 rect
		 {
			 dimensions: vec2(50, 50);
			 color: rgb(0, 0, 255, 255);
			 position: vec2(50, 0);
		 }
	 }
}
```
![green and blue squares](https://i.imgur.com/d38vXi2.png)Now there are several new things to process here. As you might have noticed, the second square's color is defined using a call to `rgb()`, but unlike in CSS where you might have expected only 3 arguments, there are actually 4 arguments, a bit like how rgba works in that language. However be assured that in this case I redundantly added the opacity value to show how it works, but it is totally optional if you want full opacity. Just like the other parameters, its max value is 255 and it's lowest is 0.

The second thing you might have noticed is that the `position` property is used in the exact same way as the `dimensions` property; you call `vec2()` and you pass it the x and y values in that order respectively.

The third thing you might have noticed is that rather than have my second `rect` right under the first one, I put it _inside_, making the first
