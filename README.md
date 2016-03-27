Asciidiagram
============

A freeform ASCII diagram parser for Haskell (inspired by Ditaa)

[![Build Status](https://travis-ci.org/Twinside/asciidiagram.png?branch=master)](https://travis-ci.org/Twinside/asciidiagram)
[![Hackage](https://img.shields.io/hackage/v/asciidiagram.svg)](http://hackage.haskell.org/package/asciidiagram)

---

Without introduction, Asciidiagram transform this:

                    /---------+
    +---------+     |         |
    |  ASCII  +---->| Diagram |
    +---------+     |         |
    |{flat}   |     +--+------/
    \---*-----/<=======/
    ::: .flat { fill: #DDD; }

into that:
![Simili logo](../master/docimages/baseExample.png?raw=true "docimages/baseExample.svg")

Lines and arrows
----------------

The basic syntax of asciidiagrams is made of lines made
outnof '-' and '|' characters. They can be connected with anchorsnlike
'+' (direct connection) or '\\' and '/' (smooth connections)n

     -----       
       -------   
                 
     |  |        
     |  |        
     |  \----    
     |           
     +-----      

![Simple lines](../master/docimages/simple_lines.png?raw=true "docimages/simple_lines.svg")

You can use dashed lines by using `:`{.haskell .identifier} for vertical
lines or `=`{.haskell .identifier} fornhorizontal lines.

     -----       
       -=-----   
                 
     |  :        
     |  |        
     |  \----    
     |           
     +--=--      

![Dashed lines](../master/docimages/dashed_lines.png?raw=true "docimages/dashed_lines.svg")

Arrows are made out of the '\<', '\>', '\^' and 'v'ncharacters.nIf the
arrows are not connected to any lines, the text is left as is.n

         ^
         |
         |
    <----+---->
         |  < > v ^
         |
         v

![Arrows](../master/docimages/arrows.png?raw=true "docimages/arrows.svg")

Shapes
------

If the lines are closed, then it is detected as such and
rendered differently

      +------+
      |      |
      |      +--+
      |      |  |
      +---+--+  |
          |     |
          +-----+

![Complex closed](../master/docimages/complexClosed.png?raw=true "docimages/complexClosed.svg")

If any of the segment posess one of the dashing markers (':' or '=')
Then the full shape will be dashed.

      +--+  +--+  +=-+  +=-+
      |  |  :  |  |  |  |  :
      +--+  +--+  +--+  +-=+

![dashing closed](../master/docimages/dashingClosed.png?raw=true "docimages/dashingClosed.svg")

Any of the angle of a shape can curved one of the smooth corner anchor
('\\' or '/')

      /--+  +--\  +--+  /--+
      |  |  |  |  |  |  |  |
      +--+  +--+  \--+  +--+

      /--+  /--\  /--+  /--\ .
      |  |  |  |  |  |  |  |
      +--/  +--+  \--/  +--/

      /--\ .
      |  |
      \--/
    .

![Curved corner](../master/docimages/curvedCorner.png?raw=true "docimages/curvedCorner.svg")

bulletdoc Adding a '\*' on a line or on a shape add a little circle on
it. If the bullet is not attached to any shape or lines, then it will be
render like any other text.

      *-*-*
      |   |  *----*
      +---/       |
              * * *

![Bullets](../master/docimages/bulletTest.png?raw=true "docimages/bulletTest.svg")

When used at connection points, it behaves like the '+' anchor.

Styles
------

The shapes can ba annotated with a tag like \`{tagname}\`.
Tags will be inserted in the class attribute of the shape and can then
be stylized with a CSS.

     +--------+         +--------+
     | Source +-------->| op1    |
     | {src}  |         \---+----/
     +--------+             |
                +-------*<--/
     +------+<--| op2   |
     | Dest |   +-------+
     |{dst} |
     +------+

    ::: .src .filled_shape { fill: #AAF; }
    ::: .dst .filled_shape { stroke: #FAA; stroke-width: 3px; }

![Styles](../master/docimages/styleExample.png?raw=true "docimages/styleExample.svg")

Inline css styles are introduced with the ":::" prefix at the beginning
of the line. They are introduced in the style section of the generated
CSS file

The generated geometry also possess some predefined class which are
overidable:

 * `dashed_elem` is applyied on every dashed element.
 * `filled_shape` is applyied on every closed shape.
 * `arrow_head` is applied on arrow head.
 * `bullet` on every bullet placed on a shape or line.
 * `line_element` on every line element, this include the arrow head.

You can then customize the appearance of the diagram as you want.

Hierarchical styles
-------------------
Starting with version 1.3, all shapes, text and lines are hierachised, a shape
within a shape will be integrated within the same group. This allows more
complex styling: 


     /------------------------------------------------------\ .
     |s100                                                  |
     |    /----------------------------\                    |
     |    |s1         /--------\       |  e1    /--------\  |
     |    |      *--->|  s2    |       +------->|  s10   |  |
     |    +----+      \---+----/       |        \--------/  |
     |    | i4 |          |            |           ^        |
     |    |{ii}+---------\| e1  {lo}   |           |        |
     |    +----+         vv            | ealarm    |        |   e0      /-------------\ .
     |    |            /--------\      +-----------/        +---------->|    s50      |
     |    +----\       | s3 {lu}|      |                    |           \-------------/
     |    | o5 |   e2  \--+-----/      |                    |
     |    |{oo}|<---------/            |<-\                 |
     |    \-+--+--------------------+--/  |                 |
     |      |                       |     | eReset          |
     |      |                       \-----/                 |
     |      v                                               |
     |  /--------\                                          |
     |  |  s20   |                  {li}                    |
     |  \--------/                                          |
     \------------------------------------------------------/
    
    ::: .li .line_element { stroke: purple; }
    ::: .li .arrow_head, .li text { fill: gray; }
    ::: .lo .line_element { stroke: blue; }
    ::: .lo .arrow_head, .lo text { fill: green; }
    ::: .lu .line_element { stroke: red; }
    ::: .lu .arrow_head, .lu text { fill: orange; }
    ::: .ii .filled_shape { fill: #DDF; }
    ::: .ii text { fill: blue; }
    ::: .oo .filled_shape { fill: #DFD; }
    ::: .oo text { fill: pink; }

![DeepStyles](../master/docimages/deepStyleExample.png?raw=true "docimages/deepStyleExample.svg")

In the previous example, we can see that the lines color are 'shape scoped' and
the tag applied to the shape above them applies to them

Shapes
------
From version 1.3, you can substitute the shape of your element with one from a
shape library. Right now the shape library is relatively small:

     +---------+  +----------+
     |         |  |          |
     | circle  |  |          |
     |         |  |    io    |
     |{circle} |  | {io}     |
     +---------+  +----------+
    
     +----------+  +----------+
     |document  |  |          |
     |          |  |          |
     |          |  | storage  |
     |{document}|  | {storage}|
     +----------+  +----------+
    
    ::: .circle .filled_shape { shape: circle; }
    ::: .document .filled_shape { shape: document; }
    ::: .storage .filled_shape { shape: storage; }
    ::: .io .filled_shape { shape: io; }

![Shape exaple](../master/docimages/shapeExample.png?raw=true "docimages/shapeExample.svg")

The mechanism use CSS styling to change the shape, if a CSS rule possess a
`shape` pseudo attribute, then the generated shape is replaced with a SVG `use`
tag with the value of the shape attribute as `href`

But, you can create your own style library and change the default stylesheet.
You can retrieve the default one with the shell command `asciidiagram
--dump-library default-lib.svg`

You can then add your own symbols tag in it and use it by calling `asciidiagram
--with-library your-lib.svg`.

