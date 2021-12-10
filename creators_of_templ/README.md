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
To run a TEMPL script, you have to open a terminal and type: `path\to\TEMPL path\to\script`. TEMPL source files have the _.css_ extension. If everything is going smoothly, you should see a window with a white background appear. But we should not keep it white, let's get some drawing done!
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

Two things to note: each property is terminated with a semi-colon and this square looks pretty lonely and bland, so let's add some more stuff! Next thing we should consider is adding some colour. Colour can be written using two ways: by using the hexadecimal notation or by calling rgb. The two methods are equivalent, however hexadecimal is faster to write if you know what you're doing and rgb is more legible and has the ability to generate colours with varying degrees of opacity while colours in hexadecimal forms are always completely opaque. First, we'll use the hexadecimal notation to have a nice dark green square:
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

The third thing you might have noticed is that rather than have my second `rect` right under the first one, I put it _inside_, making the first  first element a parent and the second a child. It'll become obvious why I did it with this:
```css
 root
 {
	 rect
	 {
		 dimensions: vec2(50, 50);
		 color: #005000;
		 position: vec2(100, 100);
		 rect
		 {
			 dimensions: vec2(50, 50);
			 color: rgb(0, 0, 255, 255);
			 position: vec2(50, 0);
		 }
	 }
}
```
![](https://i.imgur.com/fwctlpA.png)
As you can see, moving the parent element will also move all its child elements. That's because the child's element position is relative to the parent's position. Now there's just one last property left to explore and it's the rotation.

Rotation is a value in degrees by which the shape of the element is rotated. By example, if I want to a lozenge instead of my second square, I can just rotate it by 45 degrees:
```css
 root
 {
	 rect
	 {
		 dimensions: vec2(50, 50);
		 color: #005000;
		 position: vec2(100, 100);
		 rect
		 {
			 dimensions: vec2(50, 50);
			 color: rgb(0, 0, 255, 255);
			 position: vec2(50, 0);
			 rotation: 45.0;
		 }
	 }
}
```
![](https://i.imgur.com/mYUIJMQ.png)
One last remark in regards to child-parent relations that will now seem obvious is that the child will draw over the parent.

Now, we've done a lot of rectangles, but what about circles and triangles? Well first, they both share the `color`, `position` and `rotation` properties with rectangle, however the size of both the triangle and the circle is set by the value of their `radius` property, meaning all triangles int TEMPL are equilateral (all sides of the triangle are of equal length). As we've already went in detail over how to use each property, we'll only take a surface look here at a basic example and move on to operators:
```css
root
{
	circle
	{
		position: vec2(50, 25);
		radius: 25;
	}
	triangle
	{
		position: vec2(100, 25);
		radius: 25;
	}
}
```
![](https://i.imgur.com/Yn1frIP.png)

# Classes

This is where we get to the pattern part. 
The selectors are essentially what patterns are in this language.
Classes are a part of selector.

Classes are a way to group your properties:

```css
.red {
  color: #FF0000;
}
```

Now every element created with that class will have red color:

```css
root {
  circle.red {
    position: vec2(100, 100);
    radius: 50;
  }
  rect.red {
    position: vec2(150, 100);
    dimensions: vec2(50, 50);
  }
}
```

But here's the interesting part, classes can also specify what children an element will have:

```css
.with_circle {
  circle {
    radius: 10;
    color: #FF0000;
  }
}
```

Now, for instance, if we have a rectangle that has class `with_circle`, we can do the following:

```css
root {
  rect.with_circle {
    position: vec2(110, 100);
    dimensions: vec2(50, 50);
    color: #00FF00;
  }
}
```

And sure enough, we get a circle inside that rectangle, even though we didn't explicitly add it!

Now you might be thinking, can't we cause infinite recursion with that?

And sure enough, YES!


```css
.infinitely_nested {
  color: rgb(0, 0, 0, 32);
  radius: (1/$depth)*100.0;
  circle.infinitely_nested {}
}
```

Now we can add our "origin" circle

```css
root {
  circle.infinitely_nested {
    position: vec2(200, 200);
  }
}
```

Now if you do that, you can see an infinitely\* nested circle going blacker and blacker.

This is because "anything that has class .infinitely\_nested contains circle.infinitely\_nested"
Since it's a recursive definition, we get that pattern.

You also might have noticed a variable `$depth`, it's a built-in variable that tells how nested the element is.
(For example, nesting of `root` is 0)

\* Theres a recursion limit
---

Another interesting property of selectors is they match only all of the elements match

```css
.red.blue {
  color: #FF00FF;
}
```

For instance this above will trigger only if element has both `.red` and `.blue`, which assigns magenta to it.

Unfortunately this is not very powerful, as this implementation doesn't have any selector precedence, that means anyone is free to 
override the color rule. This is due to the time limit of the jam.

# Constants 

Constants can be declared at the top level of the code, and can just hold constant values

```css
$COLOR: #FF0000;

root {
  circle {
    color: $COLOR;
    position: vec2(200, 200);
    radius: 50;
  }
}
```

They can also depend on each-other

```css
$SIN30: 0.86602540378;
$SIZE: 200;
$SIDE: ($SIZE*$SIN30)*2;
```

(taken from ./examples/sierpinski.css)



