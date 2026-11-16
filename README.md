# fortfont

Fonts in Fortran: TrueType outlines and embedded bitmaps rendered at a pixel
size, and the installed fonts found without fontconfig. It builds with
gfortran and LFortran.

```fortran
use fortfont
use fortfont_find, only: font_find
type(font_t) :: font
type(glyph_t) :: g
character(len=:), allocatable :: path
integer :: face
call font_find("Selawik", "Semibold", 0, path, face)
if (font_open_file(font, path, 15, face)) then
    if (font_glyph(font, iachar("A"), g)) print *, g%width, g%height, g%advance
end if
```

- Outlines (`glyf`, simple and composite) filled with exact area coverage,
  scaled in FreeType's 26.6 fixed point so that metrics, advances and glyph
  boxes equal FreeType's. Overlapping contours are filled at four times the
  size and averaged, as FreeType does.
- Hinting (`font_set_hinting`): the font's TrueType instructions run as in
  FreeType's default interpreter (version 40, grey-scale), so hinted glyphs
  equal FreeType's and the desktop's. Fonts that FreeType auto-hints (no
  `fpgm`, at most 7 bytes of `prep`) stay unhinted.
- Embedded bitmaps: monochrome and grey strikes (EBLC/EBDT, as in `.otb`
  bitmap fonts) and colour glyphs (CBLC/CBDT, PNG through
  [fortimg](https://github.com/lazy-fortran/fortimg)).
- Collections (`.ttc`), kerning (`kern` table), names and weights.
- `font_find`: the font folders of the user and the system (or the folders in
  `FORTFONT_PATH`, separated by `:`) are scanned once,
  reading only the headers of each file; families match case-insensitively,
  styles by weight and slant, bitmap fonts by strike size. The index is cached
  in `$XDG_CACHE_HOME/fortfont-index.txt` and refreshed for new or changed files.

The outline rasteriser is a Fortran port of
[stb_truetype](https://github.com/nothings/stb) by Sean Barrett (public domain
or MIT), first written for fortplot.

## Tests

```sh
fo test        # or: fpm test
```

The tests build their fonts in memory (outline, bitmap and colour fonts) and
compare fortfont with FreeType (`libfreetype.so.6`, opened at run time, test
only): metrics, advances, kerning and glyph boxes equal, bitmap and colour
glyphs equal, outline coverage within 24 of 255. The system's DejaVu Sans and
DejaVu Sans Mono are compared as well, unhinted and hinted, and where installed
chicago95's bitmap fonts and hinted More Perfect DOS VGA, Liberation Sans and
Mono, Times New Roman and Roboto. Font discovery runs on a font folder the
test writes.
