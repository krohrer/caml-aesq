OCaml ANSI escape-sequences and text formatting - caml-aesq
======================================================================

> This library is free software; you can redistribute it and/or
> modify it under the terms of the GNU Lesser General Public
> License as published by the Free Software Foundation; either
> version 2.1 of the License, or (at your option) any later version,
> with the special exception on linking described in file LICENSE.
>
> This library is distributed in the hope that it will be useful,
> but WITHOUT ANY WARRANTY; without even the implied warranty of
> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
> Lesser General Public License for more details.
>
> You should have received a copy of the GNU Lesser General Public
> License along with this library; if not, write to the Free Software
> Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

About
----------------------------------------------------------------------

Aesq stands for ANSI escape-sequences. It's a small library that
provides a clean interface (based on the notion of text-attributes) to
generate ANSI escape-sequences. It also supports text formatting with
lazy lists composed of text elements.

Future ideas include a [Markup](http://en.wikipedia.org/wiki/Markdown)
parser and pretty printer for terminals and text modes.

[Aesq][] was originally written by Kaspar M. Rohrer (<kaspar.rohrer@gmail.com>).

Installation
----------------------------------------------------------------------

Unzip or untar in any directory, then run

    make

to generate the library and documentation. To install the library
using findlib, simply type

    make install

And to uninstall it

    make uninstall

For development, you may instead run

    sudo ln -s `pwd` `ocamlfind printconf path`/aesq

Examples
----------------------------------------------------------------------

There's not much documentation right now, Run the test by typing

> make test

and take a look at test.ml.

Of course your output device must be able to interpret ANSI
escape-sequences, otherwise the text will be garbled.

Support for hyphenation is planned. I have not yet decided on a good
interface, however. Maybe extending Text.raw with RWordBreak /
RSoftHyphen would be a good idea. And then let Text.format handle
the placement of hyphens. We'll see.

References
----------------------------------------------------------------------

* [Aesq][] - Source code repository for Aesq

[Aesq]: http://github.com/krohrer/caml-aesq "Source code repostory"