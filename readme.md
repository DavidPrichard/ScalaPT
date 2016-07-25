ScalaPT
============

Global Illumination in several hundred lines of Scala.

# Introduction

This is my fork of Jon Hanson's project ScalaPT, which is, in turn, a Scala implementation of Kevin Beason's [smallpt](http://www.kevinbeason.com/smallpt/), a simple path-tracing global illumination renderer written in C++.

Here's how he describes ScalaPT:

>Smallpt (and therefore ScalaPT) solves the [Rendering Equation](https://en.wikipedia.org/wiki/Rendering_equation)
>using a [Monte Carlo](https://en.wikipedia.org/wiki/Monte_Carlo_method) approach,
>whereby multiple light paths are fired per pixel and averaged over.
>Each path is traced backwards through the scene as it bounces off various surfaces.
>The incoming ray for each bounce is chosen at random,
>governed by the [bidirectional distribution >functions](https://en.wikipedia.org/wiki/Bidirectional_scattering_distribution_function)
>for the material of the surface in question.

>This approach, while slow to converge,
>is a relatively simple means of obtaining photorealistic images,
>which include natural effects such as ambient occlusion, light bleeding,
>reflections, refraction and caustics.

>While the application is running it displays a window containing the image as it renders:

><img src="https://github.com/jon-hanson/ScalaPT/blob/master/examples/screenshot.png" width="257"/>

Among other things, I hope to make it converge a little faster.

# Usage

The project is written entirely in Scala (v2.11.8), builds with the supplied SBT (v0.3.11) build file, and runs on Java 1.8.0.

Once built, run the `scalapt.MainFrame` class, which accepts the following optional arguments:

Parameter | Default | Description
----|----|----
inFile | scenes/cornell2.json | Filename for scene description in JSON format.
width | 1024 | Width in pixels of rendered image.
height | 768 | Height in pixels of rendered image.
frames | 1024 | Number of frames to render.
outFile | | Filename to save final image to.

* Sample scenes are provided in the scenes sub-directory.
* For the output filename, the format is inferred from the suffix.
  * Supported format types are those supported by the Java [ImageIO](https://docs.oracle.com/javase/8/docs/api/javax/imageio/ImageIO.html) write method,
which, at present, includes JPG, GIF and PNG.
  * If the file has no suffix then it defaults to PNG.

# Notes

ScalaPT differs from the original in several places:

* Each frame (or iteration) is rendered before the next, and merged into the aggregated result, to allow the image to be displayed as it is progressively refined.
* The original had what looked like a bug, whereby a bright light path could become trapped inside the glass sphere. The Russian Roulette termination would not terminate the path as the path brightness was too high, which eventually leads to a stack overflow. ScalaPT addresses this by increasing the chance of termination as the call stack depth increases.
* Infinite, one-way planes are used in place of giant spheres for the box walls.
* Scene definitions can be read from a JSON file.
* Random number generation replaced with a State monad (which wraps an XorShift random number generator).

# To-do:

* Speed up convergence by sampling with a [low-discrepancy sequence](https://en.wikipedia.org/wiki/Low-discrepancy_sequence).
* Consider any micro-optimizations worth their weight.
* 

# Examples

Hopefully I'll have some slick pictures here soon! For now, look at this ol' thing from the original:

## "Horizon"

![Horizon](https://github.com/jon-hanson/ScalaPT/blob/master/examples/horizon.png)
