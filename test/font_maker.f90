module font_maker
    !! sfnt fonts built in memory for the tests: an outline font, a monochrome
    !! bitmap-only font (as chicago95's .otb) and a colour bitmap font.
    use iso_fortran_env, only: int8
    use fortimg, only: image_t, png_encode
    implicit none
    private
    public :: outline_font, bitmap_font, colour_font, CHARS

    ! Code points of the glyphs 1..5 (glyph 0 is .notdef).
    integer, parameter :: CHARS(5) = [65, 66, 67, 68, 69]

    type :: table_t
        character(4) :: tag
        integer(int8), allocatable :: data(:)
    end type table_t

    type :: blob_t
        integer(int8), allocatable :: b(:)
    end type blob_t

contains

    function outline_font(upem) result(font)
        !! Glyphs: A a triangle with a triangular hole, B a box, C a ring of
        !! quadratic curves, D a composite of B shifted and C, E empty; a
        !! kerning pair A B.
        integer, intent(in) :: upem
        integer(int8), allocatable :: font(:)
        type(table_t) :: t(10)
        integer(int8), allocatable :: glyf(:), loca(:)
        integer :: offsets(0:6), k
        type(blob_t) :: g(0:5)
        integer :: f
        f = upem / 1000
        allocate (g(0)%b(0))
        g(1)%b = simple([2, 5], f * [100, 500, 900, 400, 500, 600], &
            f * [0, 1400, 0, 300, 900, 300], [(.true., k = 1, 6)])
        g(2)%b = simple([3], f * [100, 100, 700, 700], f * [0, 1000, 1000, 0], &
            [(.true., k = 1, 4)])
        g(3)%b = simple([7, 15], f * [500, 900, 900, 900, 500, 100, 100, 100, 500, 300, 300, &
            300, 500, 700, 700, 700], f * [0, 0, 500, 1000, 1000, 1000, 500, 0, 200, 200, &
            500, 800, 800, 800, 500, 200], [(mod(k, 2) == 1, k = 1, 16)])
        g(4)%b = composite(f)
        allocate (g(5)%b(0))
        offsets(0) = 0
        allocate (glyf(0))
        do k = 0, 5
            glyf = [glyf, g(k)%b]
            if (mod(size(glyf), 4) /= 0) glyf = [glyf, spread(0_int8, 1, 4 - mod(size(glyf), 4))]
            offsets(k + 1) = size(glyf)
        end do
        loca = [(u32(offsets(k)), k = 0, 6)]
        t(1) = table_t("OS/2", os2(upem))
        t(2) = table_t("cmap", cmap())
        t(3) = table_t("glyf", glyf)
        t(4) = table_t("head", head(upem, 1))
        t(5) = table_t("hhea", hhea(upem, 6))
        t(6) = table_t("hmtx", [u16(f * 500), u16(0), u16(f * 1000), u16(f * 100), &
            u16(f * 800), u16(f * 100), u16(f * 1000), u16(f * 100), u16(f * 1300), &
            u16(f * 100), u16(f * 600), u16(0)])
        ! kern version 0, one format-0 subtable with the pair (A, B).
        t(7) = table_t("kern", [u16(0), u16(1), u16(0), u16(20), u16(1), u16(1), u16(0), &
            u16(0), u16(0), u16(1), u16(2), s16(-150 * f)])
        t(8) = table_t("loca", loca)
        t(9) = table_t("maxp", [u32(int(z'00010000')), u16(6), (u16(64), k = 1, 13)])
        t(10) = table_t("name", names("Maker Outline", "Regular"))
        font = sfnt(t)
    end function outline_font

    function bitmap_font(ppem) result(font)
        !! A bitmap-only font with one 1-bit strike: glyphs 1..3 byte-aligned
        !! (image format 1, index format 1), glyphs 4..5 bit-packed (image
        !! format 2, index format 3); glyph k shows bars k pixels wide.
        integer, intent(in) :: ppem
        integer(int8), allocatable :: font(:)
        type(table_t) :: t(9)
        integer(int8), allocatable :: ebdt(:), eblc(:), sub1(:), sub2(:)
        integer :: k, offsets(0:5), w, h
        ebdt = u32(int(z'00020000'))
        offsets(0) = size(ebdt)
        do k = 1, 5
            w = 2 + k
            h = ppem - 2
            ! SmallGlyphMetrics: height, width, bearingX, bearingY, advance.
            ebdt = [ebdt, int([h, w, 1, ppem - 3, w + 2], int8), bars(w, h, k, k <= 3)]
            offsets(k) = size(ebdt)
        end do
        sub1 = [u16(1), u16(1), u32(offsets(0)), (u32(offsets(k) - offsets(0)), k = 0, 3)]
        sub2 = [u16(3), u16(2), u32(offsets(3)), (u16(offsets(k) - offsets(3)), k = 3, 5)]
        if (mod(size(sub2), 4) /= 0) sub2 = [sub2, u16(0)]
        ! Header, one BitmapSize record, then the IndexSubTableArray and subtables;
        ! array offsets count from the start of the array.
        eblc = [u32(int(z'00020000')), u32(1), u32(8 + 48), u32(16 + size(sub1) + size(sub2)), &
            u32(2), u32(0), line_metrics(ppem), line_metrics(ppem), u16(1), u16(5), &
            int([ppem, ppem, 1, 1], int8)]
        eblc = [eblc, u16(1), u16(3), u32(16), u16(4), u16(5), u32(16 + size(sub1)), sub1, sub2]
        t(1) = table_t("EBDT", ebdt)
        t(2) = table_t("EBLC", eblc)
        t(3) = table_t("OS/2", os2(1000))
        t(4) = table_t("cmap", cmap())
        t(5) = table_t("head", head(1000, 0))
        t(6) = table_t("hhea", hhea(1000, 6))
        t(7) = table_t("hmtx", [(u16(500), u16(0), k = 0, 5)])
        t(8) = table_t("maxp", [u32(int(z'00005000')), u16(6)])
        t(9) = table_t("name", names("Maker Bitmap", "Regular"))
        font = sfnt(t)
    end function bitmap_font

    function colour_font(ppem) result(font)
        !! A colour bitmap font: glyphs 1..5 as PNG in CBDT (image format 17).
        integer, intent(in) :: ppem
        integer(int8), allocatable :: font(:)
        type(table_t) :: t(9)
        integer(int8), allocatable :: cbdt(:), cblc(:), sub(:), png(:)
        type(image_t) :: img
        integer :: k, offsets(0:5), x, y
        cbdt = u32(int(z'00030000'))
        offsets(0) = size(cbdt)
        do k = 1, 5
            img%width = ppem - k
            img%height = ppem - 1
            img%channels = 4
            allocate (img%pixels(4, img%width, img%height))
            do y = 1, img%height
                do x = 1, img%width
                    img%pixels(:, x, y) = b8([mod(40 * k + 7 * x, 256), mod(20 * y, 256), &
                        mod(60 * k, 256), merge(255, 128, mod(x + y, 3) /= 0)])
                end do
            end do
            call png_encode(img, png)
            deallocate (img%pixels)
            cbdt = [cbdt, int([img%height, img%width, 0, ppem - 2, ppem], int8), &
                u32(size(png)), png]
            offsets(k) = size(cbdt)
        end do
        sub = [u16(1), u16(17), u32(offsets(0)), (u32(offsets(k) - offsets(0)), k = 0, 5)]
        cblc = [u32(int(z'00030000')), u32(1), u32(8 + 48), u32(8 + size(sub)), u32(1), &
            u32(0), line_metrics(ppem), line_metrics(ppem), u16(1), u16(5), &
            int([ppem, ppem, 32, 1], int8), u16(1), u16(5), u32(8), sub]
        t(1) = table_t("CBDT", cbdt)
        t(2) = table_t("CBLC", cblc)
        t(3) = table_t("OS/2", os2(1000))
        t(4) = table_t("cmap", cmap())
        t(5) = table_t("head", head(1000, 0))
        t(6) = table_t("hhea", hhea(1000, 6))
        t(7) = table_t("hmtx", [(u16(1000), u16(0), k = 0, 5)])
        t(8) = table_t("maxp", [u32(int(z'00005000')), u16(6)])
        t(9) = table_t("name", names("Maker Colour", "Regular"))
        font = sfnt(t)
    end function colour_font

    ! --- glyphs -------------------------------------------------------------

    function simple(ends, xs, ys, on) result(b)
        !! A simple glyph with absolute coordinates, stored as 16-bit deltas.
        integer, intent(in) :: ends(:), xs(:), ys(:)
        logical, intent(in) :: on(:)
        integer(int8), allocatable :: b(:)
        integer :: k
        b = [s16(size(ends)), s16(minval(xs)), s16(minval(ys)), s16(maxval(xs)), &
            s16(maxval(ys)), [(u16(ends(k)), k = 1, size(ends))], u16(0)]
        b = [b, [(int(merge(1, 0, on(k)), int8), k = 1, size(xs))]]
        b = [b, s16(xs(1)), [(s16(xs(k) - xs(k - 1)), k = 2, size(xs))]]
        b = [b, s16(ys(1)), [(s16(ys(k) - ys(k - 1)), k = 2, size(ys))]]
    end function simple

    function composite(f) result(b)
        !! Glyph 2 moved by (600, 100) and glyph 3 in place, word arguments.
        integer, intent(in) :: f
        integer(int8), allocatable :: b(:)
        b = [s16(-1), s16(100 * f), s16(0), s16(1300 * f), s16(1100 * f)]
        ! ARG_1_AND_2_ARE_WORDS | ARGS_ARE_XY_VALUES, MORE_COMPONENTS on the first.
        b = [b, u16(1 + 2 + 32), u16(2), s16(600 * f), s16(100 * f)]
        b = [b, u16(1 + 2), u16(3), s16(0), s16(0)]
    end function composite

    function bars(w, h, k, aligned) result(b)
        !! Rows of k pixels on and one off, each row byte aligned or packed.
        integer, intent(in) :: w, h, k
        logical, intent(in) :: aligned
        integer(int8), allocatable :: b(:)
        integer :: x, y, row, bit
        integer, allocatable :: v(:)
        row = w
        if (aligned) row = 8 * ((w + 7) / 8)
        allocate (v((row * h + 7) / 8))
        v = 0
        do y = 0, h - 1
            do x = 0, w - 1
                if (mod(x + y, k + 1) == k) cycle
                bit = y * row + x
                v(bit / 8 + 1) = ior(v(bit / 8 + 1), ishft(1, 7 - mod(bit, 8)))
            end do
        end do
        b = b8(v)
    end function bars

    function line_metrics(ppem) result(b)
        !! SbitLineMetrics: ascender, descender, widthMax and nine zero bytes.
        integer, intent(in) :: ppem
        integer(int8) :: b(12)
        b = 0
        b(1:3) = b8([ppem - 2, 256 - 2, ppem])
    end function line_metrics

    ! --- tables ------------------------------------------------------------

    function head(upem, loc_format) result(b)
        integer, intent(in) :: upem, loc_format
        integer(int8), allocatable :: b(:)
        integer :: k
        b = [u32(int(z'00010000')), u32(int(z'00010000')), u32(0), u32(int(z'5F0F3CF5')), &
            u16(0), u16(upem), [(u32(0), k = 1, 4)], s16(0), s16(0), s16(1300 * upem / 1000), &
            s16(1400 * upem / 1000), u16(0), u16(8), s16(2), s16(loc_format), s16(0)]
    end function head

    function hhea(upem, n) result(b)
        integer, intent(in) :: upem, n
        integer(int8), allocatable :: b(:)
        integer :: k
        b = [u32(int(z'00010000')), s16(8 * upem / 10), s16(-2 * upem / 10), s16(upem / 10), &
            u16(13 * upem / 10), s16(0), s16(0), s16(13 * upem / 10), s16(1), s16(0), s16(0), &
            [(s16(0), k = 1, 4)], s16(0), u16(n)]
    end function hhea

    function os2(upem) result(b)
        integer, intent(in) :: upem
        integer(int8), allocatable :: b(:)
        integer :: k
        b = [u16(4), s16(500), u16(400), u16(5), u16(0), [(s16(0), k = 1, 10)], s16(0), &
            s16(0), [(int(0, int8), k = 1, 10)], [(u32(0), k = 1, 4)], &
            [(int(32, int8), k = 1, 4)], u16(64), u16(65), u16(69), s16(8 * upem / 10), &
            s16(-2 * upem / 10), s16(upem / 10), u16(8 * upem / 10), u16(2 * upem / 10), &
            u32(1), u32(0), s16(upem / 2), s16(7 * upem / 10), u16(0), u16(32), u16(0)]
    end function os2

    function cmap() result(b)
        !! Format 4 (platform 3, encoding 1): A..E to glyphs 1..5.
        integer(int8), allocatable :: b(:)
        b = [u16(0), u16(1), u16(3), u16(1), u32(12)]
        b = [b, u16(4), u16(32), u16(0), u16(4), u16(4), u16(1), u16(0), &
            u16(69), u16(65535), u16(0), u16(65), u16(65535), s16(1 - 65), s16(1), &
            u16(0), u16(0)]
    end function cmap

    function names(family, style) result(b)
        character(*), intent(in) :: family, style
        integer(int8), allocatable :: b(:), strings(:)
        integer :: k
        strings = [[(int(0, int8), int(iachar(family(k:k)), int8), k = 1, len(family))], &
            [(int(0, int8), int(iachar(style(k:k)), int8), k = 1, len(style))]]
        b = [u16(0), u16(2), u16(6 + 24), &
            u16(3), u16(1), u16(1033), u16(1), u16(2 * len(family)), u16(0), &
            u16(3), u16(1), u16(1033), u16(2), u16(2 * len(style)), u16(2 * len(family)), &
            strings]
    end function names

    function sfnt(tables) result(b)
        !! The table directory (tags sorted by the caller) and padded tables.
        type(table_t), intent(in) :: tables(:)
        integer(int8), allocatable :: b(:), body(:)
        integer :: n, k, j, offset, pow, sel, pad
        n = size(tables)
        pow = 1
        sel = 0
        do while (2 * pow <= n)
            pow = 2 * pow
            sel = sel + 1
        end do
        b = [u32(int(z'00010000')), u16(n), u16(16 * pow), u16(sel), u16(16 * n - 16 * pow)]
        offset = 12 + 16 * n
        allocate (body(0))
        do k = 1, n
            pad = modulo(-size(tables(k)%data), 4)
            b = [b, [(int(iachar(tables(k)%tag(j:j)), int8), j = 1, 4)], u32(0), u32(offset), &
                u32(size(tables(k)%data))]
            body = [body, tables(k)%data, spread(0_int8, 1, pad)]
            offset = offset + size(tables(k)%data) + pad
        end do
        b = [b, body]
    end function sfnt

    pure elemental integer(int8) function b8(v)
        integer, intent(in) :: v
        b8 = int(iand(v, 255) - 256 * (iand(v, 255) / 128), int8)
    end function b8

    pure function u16(v) result(b)
        integer, intent(in) :: v
        integer(int8) :: b(2)
        b = b8([ishft(v, -8), v])
    end function u16

    pure function s16(v) result(b)
        integer, intent(in) :: v
        integer(int8) :: b(2)
        b = u16(iand(v, 65535))
    end function s16

    pure function u32(v) result(b)
        integer, intent(in) :: v
        integer(int8) :: b(4)
        b = b8([ishft(v, -24), ishft(v, -16), ishft(v, -8), v])
    end function u32

end module font_maker
