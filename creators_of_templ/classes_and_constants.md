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

Now if you do that, you can see an infinitely nested circle going blacker and blacker.

This is because "anything that has class .infinitely\_nested contains circle.infinitely\_nested"
Since it's a recursive definition, we get that pattern.

You also might have noticed a variable `$depth`, it's a built-in variable that tells how nested the element is.

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



