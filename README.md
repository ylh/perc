perc - pandoc for werc
======================

perc is a slimmed-down Pandoc-markdown-to-HTML converter suitable for use with
werc.

It accepts an optional argument to read; otherwise, it reads standard input. It
deals strictly with (extended) markdown input and HTML output; when all
transitive dependencies are built with `-split-sections`, `perc` is about half
the size of the full `pandoc` executable. In addition, it runs in the
`PandocPure` monad, ensuring no bizarre edge cases that reach out to the
outside world.

MathML for equations is turned on by default, and pure Pandoc errors/warnings
are embedded in `<pre>` tags for ease of debugging.

Licensing
---------

While the source code in this repository is permissively (ISC) licensed, keep
in mind that builds will be statically linked against Pandoc and will therefore
be under GPL v2 or (at your option) higher, the same as Pandoc.

