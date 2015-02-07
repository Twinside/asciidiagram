Asciidiagram
============

A freeform ASCII diagram parser for Haskell (inspired by Ditaa)

[![Build Status](https://travis-ci.org/Twinside/asciidiagram.png?branch=master)](https://travis-ci.org/Twinside/asciidiagram)

---

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

    ::: .src { fill: #AAF; }
    ::: .dst { stroke: #FAA; stroke-width: 3px; }

![Styles](../master/docimages/styleExample.png?raw=true "docimages/styleExample.svg")

Inline css styles are introduced with the ":::" prefix at the beginning
of the line. They are introduced in the style section of the generated
CSS file

The generated geometry also possess some predefined class which are
overidable:

 * `dashed\_elem` is applyied on every dashed element.
 * `filled\_shape` is applyied on every closed shape.
 * `bullet` on every bullet placed on a shape or line.
 * `line\_element` on every line element, this include the arrow head.

You can then customize the appearance of the diagram as you want.

