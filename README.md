diagrams-reflex  [![Build Status](https://travis-ci.org/diagrams/diagrams-reflex.png?branch=master)](http://travis-ci.org/diagrams/diagrams-reflex)
------------


_diagrams-reflex_ is a an SVG backend for [diagrams], to be used in
the browser via [ghcjs]. Diagrams is a powerful, flexible, declarative
domain-specific language for creating vector graphics, using the
[Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell
[ghcjs]: https://github.com/ghcjs/ghcjs

# Installation

```
git clone git@github.com:diagrams/diagrams-reflex
cd diagrams-reflex
```

Pick one of the build methods below.

## With stack

```
stack build
```

## with [reflex-platform](https://github.com/reflex-frp/reflex-platform) ##

```
cd examples
work-on ./ghcjs.nix ./.
```

The `work-on` script can be found [in the top-level directory of reflex-platform](https://github.com/reflex-frp/reflex-platform/blob/develop/work-on).

# Examples

The examples directory contains several simple examples.  Running
versions of these examples (and others, not all using `reflex`) are
online at
[http://bergey.github.io/gooey](http://bergey.github.io/gooey).

# Capabilities

The following features are supported.  If they don't work as expected
(or as other Diagrams Backends), please file a bug report.

    - fill color (solid only)
    - line color
    - line width
    - line cap & join
    - dashing
    - opacity
    - Paths
    - Text
    - mouse events
    - font weight

These features are not yet implemented.  Pull requests welcome!

    - textures
