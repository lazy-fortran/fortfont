module fortfont
    !! Fonts in Fortran: sfnt outlines and embedded bitmaps rendered at a pixel
    !! size, with FreeType's metric rounding (26.6 fixed point), so layouts
    !! measured against FreeType keep their positions.
    use iso_fortran_env, only: int8, int64, dp => real64
    use fortfont_sfnt, only: sfnt_t, strike_t, sbit_t, sfnt_open, sfnt_glyph, sfnt_advance, &
        sfnt_faces, sfnt_bitmap, sfnt_kerning
    use fortfont_tt_outlines, only: tt_vertex_t, tt_get_glyph_shape
    use fortfont_tt_curves, only: tt_point_t, tt_flatten_curves
    use fortfont_tt_rasterizer, only: tt_rasterize
    use fortimg, only: image_t, png_decode, image_convert, IMG_OK
    implicit none
    private
    public :: font_t, glyph_t, font_open, font_open_file, font_set_size, font_has, &
        font_glyph, font_advance, font_kerning, font_faces_in_file
    public :: sfnt_t

    type :: glyph_t
        !! A rendered glyph: coverage (or colour) and metrics in whole pixels.
        !! `top` is the distance from the baseline up to the first row.
        integer :: width = 0, height = 0, left = 0, top = 0, advance = 0
        logical :: colour = .false.
        integer(int8), allocatable :: alpha(:, :) ! (width, height), coverage 0..255
        integer(int8), allocatable :: rgba(:, :, :) ! (4, width, height) for colour glyphs
    end type glyph_t

    type :: font_t
        type(sfnt_t) :: sfnt
        integer :: size = 0 ! pixels per em
        integer :: strike = 0 ! embedded bitmap strike in use, 0 for outlines
        integer :: ascent = 0, descent = 0, height = 0, line_skip = 0
        integer(int64) :: x_scale = 0, y_scale = 0 ! 16.16, as FreeType's size metrics
    end type font_t

contains

    integer function font_faces_in_file(data) result(n)
        integer(int8), intent(in) :: data(:)
        n = sfnt_faces(data)
    end function font_faces_in_file

    logical function font_open(font, data, pixels, face) result(ok)
        !! A font from the bytes of a font file at `pixels` per em.
        type(font_t), intent(out) :: font
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: pixels
        integer, intent(in), optional :: face
        ok = sfnt_open(font%sfnt, data, face)
        if (ok) call font_set_size(font, pixels)
    end function font_open

    logical function font_open_file(font, path, pixels, face) result(ok)
        type(font_t), intent(out) :: font
        character(*), intent(in) :: path
        integer, intent(in) :: pixels
        integer, intent(in), optional :: face
        integer(int8), allocatable :: data(:)
        integer :: u, n, ios
        ok = .false.
        open (newunit=u, file=path, access="stream", form="unformatted", status="old", &
            action="read", iostat=ios)
        if (ios /= 0) return
        inquire (unit=u, size=n)
        allocate (data(max(n, 0)))
        if (n > 0) read (u, iostat=ios) data
        close (u)
        if (ios /= 0) return
        ok = font_open(font, data, pixels, face)
    end function font_open_file

    subroutine font_set_size(font, pixels)
        !! Pixels per em. A bitmap-only font takes its nearest strike; an outline
        !! font takes a strike of exactly this size when it has one (as FreeType).
        type(font_t), intent(inout) :: font
        integer, intent(in) :: pixels
        integer :: k, best
        integer(int64) :: upem
        font%size = max(1, pixels)
        font%strike = 0
        best = 0
        if (allocated(font%sfnt%strikes)) then
            do k = 1, size(font%sfnt%strikes)
                if (font%sfnt%strikes(k)%ppem == font%size) then
                    best = k
                    exit
                end if
                if (.not. font%sfnt%outlines) then
                    if (best == 0) then
                        best = k
                    else if (abs(font%sfnt%strikes(k)%ppem - font%size) < &
                            abs(font%sfnt%strikes(best)%ppem - font%size)) then
                        best = k
                    end if
                end if
            end do
        end if
        upem = max(1, font%sfnt%units_per_em)
        if (best > 0 .and. .not. font%sfnt%outlines) then
            font%strike = best
            associate (s => font%sfnt%strikes(best))
                font%ascent = s%ascender
                font%descent = s%descender
                font%height = s%ascender - s%descender
                font%line_skip = font%height
                font%x_scale = div_fix(int(s%ppem, int64) * 64, upem)
                font%y_scale = font%x_scale
            end associate
            return
        end if
        if (best > 0) font%strike = best
        ! FreeType: y_scale = FT_DivFix(ppem << 6, units_per_em), then the
        ! scaled metrics rounded up to whole pixels as SDL_ttf reads them.
        font%x_scale = div_fix(int(font%size, int64) * 64, upem)
        font%y_scale = font%x_scale
        font%ascent = ceil26(mul_fix(int(font%sfnt%ascender, int64), font%y_scale))
        font%descent = ceil26(mul_fix(int(font%sfnt%descender, int64), font%y_scale))
        font%height = ceil26(mul_fix(int(font%sfnt%ascender - font%sfnt%descender, int64), &
            font%y_scale))
        font%line_skip = ceil26(mul_fix(int(font%sfnt%ascender - font%sfnt%descender + &
            font%sfnt%line_gap, int64), font%y_scale))
    end subroutine font_set_size

    logical function font_has(font, codepoint)
        type(font_t), intent(in) :: font
        integer, intent(in) :: codepoint
        font_has = sfnt_glyph(font%sfnt, codepoint) > 0
    end function font_has

    integer function font_advance(font, codepoint) result(adv)
        !! The advance in whole pixels, rounded as hinted FreeType advances are.
        type(font_t), intent(in) :: font
        integer, intent(in) :: codepoint
        type(sbit_t) :: sbit
        integer :: g, a, lsb
        g = sfnt_glyph(font%sfnt, codepoint)
        if (font%strike > 0 .and. .not. font%sfnt%outlines) then
            adv = 0
            if (sfnt_bitmap(font%sfnt, font%sfnt%strikes(font%strike), g, sbit)) adv = sbit%advance
            return
        end if
        call sfnt_advance(font%sfnt, g, a, lsb)
        adv = round26(mul_fix(int(a, int64), font%x_scale))
    end function font_advance

    integer function font_kerning(font, left, right) result(k)
        !! Kerning between two code points in whole pixels (kern table).
        type(font_t), intent(in) :: font
        integer, intent(in) :: left, right
        integer :: a, b
        a = sfnt_glyph(font%sfnt, left)
        b = sfnt_glyph(font%sfnt, right)
        k = round26(mul_fix(int(sfnt_kerning(font%sfnt, a, b), int64), font%x_scale))
    end function font_kerning

    logical function font_glyph(font, codepoint, glyph) result(found)
        !! The rendered glyph of a code point; false when the font lacks it
        !! (glyph is then the font's .notdef glyph).
        type(font_t), intent(in) :: font
        integer, intent(in) :: codepoint
        type(glyph_t), intent(out) :: glyph
        integer :: g
        g = sfnt_glyph(font%sfnt, codepoint)
        found = g > 0
        if (font%strike > 0) then
            if (bitmap_glyph(font, g, glyph)) return
        end if
        if (font%sfnt%outlines) call outline_glyph(font, g, glyph)
    end function font_glyph

    logical function bitmap_glyph(font, g, glyph) result(ok)
        type(font_t), intent(in) :: font
        integer, intent(in) :: g
        type(glyph_t), intent(inout) :: glyph
        type(sbit_t) :: sbit
        type(image_t) :: img
        integer :: status
        real(dp) :: f
        ok = sfnt_bitmap(font%sfnt, font%sfnt%strikes(font%strike), g, sbit)
        if (.not. ok) return
        if (.not. sbit%colour) then
            glyph%width = sbit%width
            glyph%height = sbit%height
            glyph%left = sbit%left
            glyph%top = sbit%top
            glyph%advance = sbit%advance
            if (allocated(sbit%alpha)) then
                call move_alloc(sbit%alpha, glyph%alpha)
            else
                allocate (glyph%alpha(0, 0))
            end if
            return
        end if
        call png_decode(sbit%png, img, status)
        ok = status == IMG_OK
        if (.not. ok) return
        call image_convert(img, 4)
        ! Colour strikes come in one size: scale the picture to the font size.
        f = real(font%size, dp) / max(1, font%sfnt%strikes(font%strike)%ppem)
        glyph%colour = .true.
        glyph%width = max(1, nint(img%width * f))
        glyph%height = max(1, nint(img%height * f))
        glyph%left = nint(sbit%left * f)
        glyph%top = nint(sbit%top * f)
        glyph%advance = nint(sbit%advance * f)
        glyph%rgba = box_scale(img%pixels, glyph%width, glyph%height)
    end function bitmap_glyph

    subroutine outline_glyph(font, g, glyph)
        !! The outline scaled to 26.6 fixed point as FreeType scales it, its box
        !! rounded out to whole pixels, filled with exact area coverage
        !! (stb_truetype's rasteriser) after flattening curves to 1/16 pixel.
        type(font_t), intent(in) :: font
        integer, intent(in) :: g
        type(glyph_t), intent(inout) :: glyph
        type(tt_vertex_t), allocatable :: v(:)
        type(tt_point_t), allocatable :: pts(:)
        integer, allocatable :: wcount(:)
        integer :: nv, nw, a, lsb, k, x0, y0, x1, y1, minx, maxx, miny, maxy
        integer(int8), allocatable :: buf(:)
        real(dp), parameter :: S = 1.0_dp / 64
        call sfnt_advance(font%sfnt, g, a, lsb)
        glyph%advance = round26(mul_fix(int(a, int64), font%x_scale))
        call tt_get_glyph_shape(font%sfnt%data, font%sfnt%loca, font%sfnt%glyf, &
            font%sfnt%loc_format, font%sfnt%num_glyphs, g, v, nv)
        allocate (glyph%alpha(0, 0))
        if (nv <= 0) return
        do k = 1, nv
            v(k)%x = int(mul_fix(int(v(k)%x, int64), font%x_scale))
            v(k)%y = int(mul_fix(int(v(k)%y, int64), font%y_scale))
            v(k)%cx = int(mul_fix(int(v(k)%cx, int64), font%x_scale))
            v(k)%cy = int(mul_fix(int(v(k)%cy, int64), font%y_scale))
        end do
        ! The control box: on-curve points and the control points of curves.
        minx = huge(1)
        maxx = -huge(1)
        miny = huge(1)
        maxy = -huge(1)
        do k = 1, nv
            minx = min(minx, v(k)%x)
            maxx = max(maxx, v(k)%x)
            miny = min(miny, v(k)%y)
            maxy = max(maxy, v(k)%y)
            if (v(k)%vtype == 3) then
                minx = min(minx, v(k)%cx)
                maxx = max(maxx, v(k)%cx)
                miny = min(miny, v(k)%cy)
                maxy = max(maxy, v(k)%cy)
            end if
        end do
        x0 = int(floor_div(int(minx, int64), 64_int64))
        x1 = int(floor_div(int(maxx, int64) + 63, 64_int64))
        y0 = int(floor_div(int(-maxy, int64), 64_int64))
        y1 = int(floor_div(int(-miny, int64) + 63, 64_int64))
        glyph%width = max(0, x1 - x0)
        glyph%height = max(0, y1 - y0)
        glyph%left = x0
        glyph%top = -y0
        if (glyph%width == 0 .or. glyph%height == 0) return
        call tt_flatten_curves(v, nv, 64.0_dp / 16, pts, wcount, nw)
        allocate (buf(glyph%width * glyph%height))
        buf = 0
        call tt_rasterize(buf, glyph%width, glyph%height, glyph%width, pts, wcount, nw, S, S, &
            0.0_dp, 0.0_dp, x0, y0, .true.)
        deallocate (glyph%alpha)
        glyph%alpha = reshape(buf, [glyph%width, glyph%height])
    end subroutine outline_glyph

    function box_scale(src, w, h) result(out)
        !! RGBA scaled to w x h by averaging the source pixels each one covers.
        integer(int8), intent(in) :: src(:, :, :)
        integer, intent(in) :: w, h
        integer(int8) :: out(4, w, h)
        integer :: x, y, sx0, sx1, sy0, sy1, i, j, c, n
        integer :: acc(4)
        do y = 1, h
            sy0 = (y - 1) * size(src, 3) / h + 1
            sy1 = max(sy0, y * size(src, 3) / h)
            do x = 1, w
                sx0 = (x - 1) * size(src, 2) / w + 1
                sx1 = max(sx0, x * size(src, 2) / w)
                acc = 0
                n = 0
                do j = sy0, sy1
                    do i = sx0, sx1
                        do c = 1, 4
                            acc(c) = acc(c) + iand(int(src(c, i, j)), 255)
                        end do
                        n = n + 1
                    end do
                end do
                do c = 1, 4
                    out(c, x, y) = int((acc(c) + n / 2) / n - 256 * (((acc(c) + n / 2) / n) / 128), &
                        int8)
                end do
            end do
        end do
    end function box_scale

    ! --- FreeType's fixed-point arithmetic ----------------------------------

    pure integer(int64) function div_fix(a, b)
        !! FT_DivFix: (a << 16) / b, rounded.
        integer(int64), intent(in) :: a, b
        div_fix = (a * 65536 + b / 2) / b
    end function div_fix

    pure integer(int64) function mul_fix(a, b)
        !! FT_MulFix: (a * b) >> 16, rounded half away from zero.
        integer(int64), intent(in) :: a, b
        integer(int64) :: p
        p = abs(a) * abs(b)
        mul_fix = (p + 32768) / 65536
        if ((a < 0) .neqv. (b < 0)) mul_fix = -mul_fix
    end function mul_fix

    pure integer function ceil26(v)
        !! FT_CEIL: a 26.6 value rounded up to whole pixels.
        integer(int64), intent(in) :: v
        ceil26 = int(floor_div(v + 63, 64_int64))
    end function ceil26

    pure integer function round26(v)
        !! FT_PIX_ROUND: a 26.6 value to the nearest whole pixel.
        integer(int64), intent(in) :: v
        round26 = int(floor_div(v + 32, 64_int64))
    end function round26

    pure integer(int64) function floor_div(a, b)
        integer(int64), intent(in) :: a, b
        floor_div = (a - modulo(a, b)) / b
    end function floor_div

end module fortfont
