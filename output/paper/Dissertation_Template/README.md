# Tulane SSE Math Dissertation Template

This is a complete, Overleaf-compatible LaTeX template designed specifically for dissertations and master's theses submitted to the School of Science and Engineering (SSE) at Tulane University, with a specific focus on Mathematics formatting.

## Authorship & Credits

- **Original 1999 Architecture**: The core spacing and layout engine (`TUT.cls`) and bibliography style (`TUT.bst`) were originally authored by **Dmitri Alexeev** (alexeev@member.ams.org).
- **2026 Modernization**: The template was extensively audited, modernized, and patched by **Sam Punshon-Smith** (spunshonsmith@tulane.edu) to natively inject the latest strict SSE formatting rules (2-inch margins, word counts, electronic signatures, and CTAN package compatibility like `hyperref` and `endnotes`).

## Overview

This template adheres strictly to the Tulane SSE "GENERAL GUIDELINES FOR USE IN PREPARING THESES AND DISSERTATIONS." It enforces:

- The required 1.5-inch left margin and 1-inch top/bottom/right margins.
- The 2-inch top margin drop for all new chapters, table of contents, lists, and backmatter.
- Roman numeral pagination for preliminary pages (bottom centered) and Arabic numeral pagination for the main text (top right).
- Proper spacing for block quotes, footnotes, tables, and figures.

## How to Use

1. **Main Document (`main.tex`)**: This is the orchestrator file. You compile this file using `pdflatex main.tex`. Fill in your name, committee members, and approval dates here using the `\author{}`, `\membera{}`, etc. commands.
2. **Style Configuration (`TUT.cls`)**: DO NOT modify this file unless you know what you are doing. This is the massive legacy class wrapper authored by Dmitri Alexeev that handles all margin patching, title page generation, and university hierarchy rules native to Tulane.
3. **Bibliography (`TUT.bst` & `bib.bib`)**: The legacy bibliography style `TUT.bst` (also by Dmitri Alexeev) applies the AMSplain standard format. Put your references in `bib.bib` and use `\bibliographystyle{TUT}`.
4. **Chapters**: Add your chapters as separate `.tex` files (like `intro.tex` and `examples.tex`) and `\input{}` them into `main.tex`.

## Built-in Features

- **Glossaries and Indices**: The template natively supports the `glossaries` and `imakeidx` packages. Define your terms and use `\makeglossaries` / `\makeindex` in the preamble.
- **Endnotes**: The `endnotes` package is included and automatically configured to obey the 2-inch top margin rule.
- **Figures & Tables**: Floating objects (`\begin{figure}`, `\begin{table}`) should be manually spaced with three blank lines above and below if they fall in the middle of a text page, per the guidelines.

## Compilation Sequence

To fully compile the document with all features (bibliography, index, glossary), use the following sequence:

```bash
pdflatex main.tex
bibtex main           # (if you are citing anything)
makeindex main.idx
makeglossaries main
pdflatex main.tex
pdflatex main.tex
```

**Alternative (latexmk):**
If you have `latexmk` installed, you can simply run:

```bash
latexmk -pdf main.tex
```

This will automatically run `pdflatex`, `bibtex`, and the indexing engines as many times as necessary to resolve all cross-references.

## Overleaf Usage

This template was specifically designed using standard CTAN packages so that it is **100% compatible with Overleaf**.

To use this template on Overleaf:

1. Zip all the files in this directory (including the `.sty` file).
2. Create a new project in Overleaf and select **Upload Project**.
3. Upload the zip file.
4. Open `main.tex` and compile! Overleaf will automatically handle the compilation engine (like `latexmk`) and generate your PDF.
