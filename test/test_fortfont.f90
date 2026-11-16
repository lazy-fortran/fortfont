program test_fortfont
    !! Fonts built here and the system's DejaVu fonts, rendered by fortfont and
    !! by FreeType (opened at run time): equal metrics, advances, kerning and
    !! glyph boxes; equal bitmap and colour glyphs; outline coverage within a
    !! small tolerance (both compute exact area coverage, in different order).
    !! Hinted by the fonts' own TrueType instructions, glyphs equal FreeType's
    !! default (native, v40) hinting in box and, within the tolerance, coverage.
    use iso_fortran_env, only: int8, int64
    use fortfont
    use font_maker, only: outline_font, bitmap_font, colour_font, CHARS
    use freetype_reference
    implicit none
    ! Largest coverage difference per pixel allowed against FreeType, 0..255.
    integer, parameter :: COVERAGE_TOLERANCE = 24
    integer :: failures, checked, k, px
    integer, parameter :: SIZES(6) = [9, 11, 13, 16, 24, 40]
    character(*), parameter :: SYSTEM_FONTS(4) = [character(64) :: &
        "/usr/share/fonts/TTF/DejaVuSans.ttf", "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", &
        "/usr/share/fonts/TTF/DejaVuSansMono.ttf", &
        "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"]
    character(*), parameter :: OPTIONAL_BITMAP_FONTS(3) = [character(64) :: &
        "/usr/share/fonts/misc/chicago95/crox1h.otb", &
        "/usr/share/fonts/misc/chicago95/crox1hb.otb", "/usr/share/fonts/misc/chicago95/crox4hb.otb"]
    ! TrueType fonts with instructions, where installed: a DOS VGA font on a
    ! pixel grid, fonts whose programs ask GETINFO for the smoothing (Liberation),
    ! flip points (Times) or overlap their contours (Roboto).
    character(*), parameter :: OPTIONAL_HINTED_FONTS(5) = [character(64) :: &
        "/usr/share/fonts/TTF/MorePerfectDOSVGA.ttf", &
        "/usr/share/fonts/liberation/LiberationSans-Regular.ttf", &
        "/usr/share/fonts/liberation/LiberationMono-Regular.ttf", &
        "/usr/share/fonts/TTF/Times.TTF", &
        "/usr/share/fonts/TTF/Roboto-Regular.ttf"]
    integer :: worst_coverage, found_system, j
    logical :: exists

    failures = 0
    checked = 0
    worst_coverage = 0
    if (.not. freetype_open()) error stop "the reference FreeType (libfreetype.so.6) is missing"
    do k = 1, size(SIZES)
        px = SIZES(k)
        call compare_outlines("outline font, 1000 units", outline_font(1000), px, CHARS, .true.)
        call compare_outlines("outline font, 2048 units", outline_font(2048), px, CHARS, .true.)
    end do
    call compare_bitmaps("bitmap font 11 px", bitmap_font(11), 11)
    call compare_bitmaps("bitmap font 16 px", bitmap_font(16), 16)
    call compare_bitmaps("bitmap font with empty glyf, 11 px", bitmap_font(11, .true.), 11)
    call compare_colour("colour font 20 px", colour_font(20), 20)
    found_system = 0
    do k = 1, size(SYSTEM_FONTS)
        inquire (file=trim(SYSTEM_FONTS(k)), exist=exists)
        if (.not. exists) cycle
        found_system = found_system + 1
        do px = 9, 24
            call compare_outlines(trim(SYSTEM_FONTS(k)), read_file(trim(SYSTEM_FONTS(k))), px, &
                [(j, j = 32, 126), 228, 246, 252, 223, 8364, 9472], .false.)
        end do
    end do
    call check(found_system >= 2, "DejaVu Sans and DejaVu Sans Mono are installed")
    do k = 1, size(SYSTEM_FONTS)
        inquire (file=trim(SYSTEM_FONTS(k)), exist=exists)
        if (.not. exists) cycle
        do px = 9, 24
            call compare_hinted(trim(SYSTEM_FONTS(k)), read_file(trim(SYSTEM_FONTS(k))), px, &
                [(j, j = 32, 126), 228, 246, 252, 223, 8364])
        end do
    end do
    do k = 1, size(OPTIONAL_HINTED_FONTS)
        inquire (file=trim(OPTIONAL_HINTED_FONTS(k)), exist=exists)
        if (.not. exists) cycle
        do px = 12, 24
            call compare_hinted(trim(OPTIONAL_HINTED_FONTS(k)), &
                read_file(trim(OPTIONAL_HINTED_FONTS(k))), px, [(j, j = 32, 126)])
        end do
    end do
    ! Real bitmap and colour fonts, where installed: chicago95 (classic theme)
    ! and Noto Color Emoji; the generated fonts cover their formats everywhere.
    do k = 1, size(OPTIONAL_BITMAP_FONTS)
        inquire (file=trim(OPTIONAL_BITMAP_FONTS(k)), exist=exists)
        if (exists) call compare_real_bitmaps(trim(OPTIONAL_BITMAP_FONTS(k)))
    end do
    write (*, '(a, i0)') "largest outline coverage difference: ", worst_coverage
    write (*, '(a, i0, a, i0, a)') "fortfont: ", checked, " checks, ", failures, " failures"
    if (failures > 0) error stop 1

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        checked = checked + 1
        if (ok) return
        failures = failures + 1
        write (*, '(a)') "FAIL: " // what
    end subroutine check

    subroutine compare_outlines(name, data, px, codepoints, kern)
        character(*), intent(in) :: name
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: px, codepoints(:)
        logical, intent(in) :: kern
        type(font_t) :: font
        type(ft_face_t) :: face
        type(glyph_t) :: g
        type(ft_glyph_t) :: ref
        integer(int64) :: y_scale, a26, d26, h26
        integer :: upem, asc, desc, k, d, left, width
        character(80) :: what
        write (what, '(a, a, i0, a)') name, " at ", px, " px"
        call check(font_open(font, data, px), trim(what) // ": opens")
        call check(ft_face_open(face, data), trim(what) // ": FreeType opens")
        call check(ft_set_size(face, px), trim(what) // ": FreeType size")
        call ft_metrics(face, y_scale, a26, d26, h26)
        call ft_units(face, upem, asc, desc)
        call check(font%y_scale == y_scale, trim(what) // ": scale")
        call check(font%ascent == ceil_div(scaled(asc, y_scale), 64) .and. &
            font%descent == ceil_div(scaled(desc, y_scale), 64) .and. &
            font%height == ceil_div(scaled(asc - desc, y_scale), 64), trim(what) // ": metrics")
        do k = 1, size(codepoints)
            if (.not. ft_load(face, codepoints(k), FT_LOAD_TARGET_LIGHT, ref)) cycle
            call check(font_advance(font, codepoints(k)) * 64 == ref%advance26, &
                trim(what) // ": advance of " // str(codepoints(k)))
            call check(font_advance26(font, codepoints(k), .true.) == &
                ft_fast_advance(face, codepoints(k)), trim(what) // &
                ": fractional advance of " // str(codepoints(k)))
            if (.not. ft_load(face, codepoints(k), FT_LOAD_RENDER + FT_LOAD_NO_HINTING, ref)) cycle
            if (.not. font_glyph(font, codepoints(k), g)) cycle
            call font_glyph_box(font, codepoints(k), left, width)
            call check(left == g%left .and. (width == g%width .or. g%height == 0), trim(what) // &
                ": ink box without rendering of " // str(codepoints(k)))
            call check(g%width == ref%width .and. g%height == ref%height .and. &
                g%left == ref%left .and. g%top == ref%top, trim(what) // ": box of " // &
                str(codepoints(k)) // " is " // box(g%width, g%height, g%left, g%top) // &
                ", FreeType " // box(ref%width, ref%height, ref%left, ref%top))
            if (g%width /= ref%width .or. g%height /= ref%height .or. g%width == 0) cycle
            d = maxval(abs(u8(g%alpha) - u8(ref%pixels(1, :, :))))
            worst_coverage = max(worst_coverage, d)
            call check(d <= COVERAGE_TOLERANCE, trim(what) // ": coverage of " // &
                str(codepoints(k)) // " differs by " // str(d))
        end do
        if (kern) call check(font_kerning(font, 65, 66) * 64 == &
            64 * floor((ft_kerning(face, 65, 66) + 32) / 64.0), &
            trim(what) // ": kerning")
    end subroutine compare_outlines

    subroutine compare_hinted(name, data, px, codepoints)
        !! fortfont with the font's instructions against FreeType's default
        !! hinting: the same glyph box, coverage within the tolerance.
        character(*), intent(in) :: name
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: px, codepoints(:)
        type(font_t) :: font
        type(ft_face_t) :: face
        type(glyph_t) :: g
        type(ft_glyph_t) :: ref
        integer :: k, d
        character(96) :: what
        write (what, '(a, a, i0, a)') name, " hinted at ", px, " px"
        call check(font_open(font, data, px), trim(what) // ": opens")
        call font_set_hinting(font, .true.)
        call check(font%hinted, trim(what) // ": runs the font's instructions")
        call check(ft_face_open(face, data), trim(what) // ": FreeType opens")
        call check(ft_set_size(face, px), trim(what) // ": FreeType size")
        do k = 1, size(codepoints)
            if (.not. ft_load(face, codepoints(k), FT_LOAD_RENDER + FT_LOAD_DEFAULT, ref)) cycle
            if (.not. font_glyph(font, codepoints(k), g)) cycle
            call check(g%width == ref%width .and. g%height == ref%height .and. &
                g%left == ref%left .and. g%top == ref%top, trim(what) // ": box of " // &
                str(codepoints(k)) // " is " // box(g%width, g%height, g%left, g%top) // &
                ", FreeType " // box(ref%width, ref%height, ref%left, ref%top))
            if (g%width /= ref%width .or. g%height /= ref%height .or. g%width == 0) cycle
            d = maxval(abs(u8(g%alpha) - u8(ref%pixels(1, :, :))))
            worst_coverage = max(worst_coverage, d)
            call check(d <= COVERAGE_TOLERANCE, trim(what) // ": coverage of " // &
                str(codepoints(k)) // " differs by " // str(d))
        end do
    end subroutine compare_hinted

    subroutine compare_bitmaps(name, data, px)
        character(*), intent(in) :: name
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: px
        type(font_t) :: font
        type(ft_face_t) :: face
        type(glyph_t) :: g
        type(ft_glyph_t) :: ref
        integer(int64) :: y_scale, a26, d26, h26
        integer :: k
        call check(font_open(font, data, px), name // ": opens")
        call check(ft_face_open(face, data) .and. ft_set_size(face, px), name // &
            ": FreeType opens")
        call ft_metrics(face, y_scale, a26, d26, h26)
        call check(font%ascent * 64 == a26 .and. font%descent * 64 == d26 .and. &
            font%height * 64 == h26, name // ": strike metrics")
        do k = 1, size(CHARS)
            call check(font_glyph(font, CHARS(k), g), name // ": glyph present")
            call check(ft_load(face, CHARS(k), FT_LOAD_RENDER, ref), name // ": FreeType glyph")
            call check(g%width == ref%width .and. g%height == ref%height .and. &
                g%left == ref%left .and. g%top == ref%top .and. g%advance * 64 == ref%advance26, &
                name // ": box of " // str(CHARS(k)))
            if (g%width /= ref%width .or. g%height /= ref%height) cycle
            call check(all(g%alpha == ref%pixels(1, :, :)), name // ": pixels of " // &
                str(CHARS(k)))
        end do
    end subroutine compare_bitmaps

    subroutine compare_colour(name, data, px)
        !! FreeType gives premultiplied BGRA; ours is straight RGBA.
        character(*), intent(in) :: name
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: px
        type(font_t) :: font
        type(ft_face_t) :: face
        type(glyph_t) :: g
        type(ft_glyph_t) :: ref
        integer :: k, x, y, c, a, worst
        call check(font_open(font, data, px), name // ": opens")
        call check(ft_face_open(face, data) .and. ft_set_size(face, px), name // &
            ": FreeType opens")
        worst = 0
        do k = 1, size(CHARS)
            call check(font_glyph(font, CHARS(k), g) .and. g%colour, name // ": colour glyph")
            call check(ft_load(face, CHARS(k), FT_LOAD_COLOR + FT_LOAD_RENDER, ref) .and. &
                ref%mode == 7, name // ": FreeType colour glyph")
            call check(g%width == ref%width .and. g%height == ref%height .and. &
                g%left == ref%left .and. g%top == ref%top, name // ": box of " // str(CHARS(k)))
            if (g%width /= ref%width .or. g%height /= ref%height) cycle
            do y = 1, g%height
                do x = 1, g%width
                    a = u8(g%rgba(4, x, y))
                    do c = 1, 3
                        worst = max(worst, abs(u8(g%rgba(c, x, y)) * a / 255 - &
                            u8(ref%pixels(4 - c, x, y))))
                    end do
                    worst = max(worst, abs(a - u8(ref%pixels(4, x, y))))
                end do
            end do
        end do
        call check(worst <= 1, name // ": pixels within 1 of FreeType")
    end subroutine compare_colour

    subroutine compare_real_bitmaps(path)
        !! Every printable ASCII and Latin-1 glyph of a bitmap font file.
        character(*), intent(in) :: path
        type(font_t) :: font
        type(ft_face_t) :: face
        type(glyph_t) :: g
        type(ft_glyph_t) :: ref
        integer(int8), allocatable :: data(:)
        integer :: c, ppem
        data = read_file(path)
        call check(font_open(font, data, 11), path // ": opens")
        ppem = font%sfnt%strikes(1)%ppem
        call font_set_size(font, ppem)
        call check(font%strike > 0, path // ": strike in use")
        call check(ft_face_open(face, data) .and. ft_set_size(face, ppem), path // &
            ": FreeType opens")
        do c = 32, 255
            if (c > 126 .and. c < 160) cycle
            if (.not. font_has(font, c)) cycle
            if (.not. font_glyph(font, c, g)) cycle
            call check(ft_load(face, c, FT_LOAD_RENDER, ref), path // ": FreeType glyph")
            call check(g%width == ref%width .and. g%height == ref%height .and. &
                g%left == ref%left .and. g%top == ref%top .and. g%advance * 64 == ref%advance26, &
                path // ": box of " // str(c))
            if (g%width /= ref%width .or. g%height /= ref%height) cycle
            call check(all(g%alpha == ref%pixels(1, :, :)), path // ": pixels of " // str(c))
        end do
    end subroutine compare_real_bitmaps

    pure integer(int64) function scaled(v, s)
        integer, intent(in) :: v
        integer(int64), intent(in) :: s
        integer(int64) :: p
        p = abs(int(v, int64)) * abs(s)
        scaled = (p + 32768) / 65536
        if ((v < 0) .neqv. (s < 0)) scaled = -scaled
    end function scaled

    pure integer function ceil_div(a, b)
        integer(int64), intent(in) :: a
        integer, intent(in) :: b
        ceil_div = int(-floor(real(-a, 8) / b))
    end function ceil_div

    elemental integer function u8(b)
        integer(int8), intent(in) :: b
        u8 = iand(int(b), 255)
    end function u8

    function box(w, h, l, t) result(s)
        integer, intent(in) :: w, h, l, t
        character(:), allocatable :: s
        s = str(w) // "x" // str(h) // "@" // str(l) // "," // str(t)
    end function box

    function str(i) result(s)
        integer, intent(in) :: i
        character(:), allocatable :: s
        character(12) :: b
        write (b, '(i0)') i
        s = trim(b)
    end function str

    function read_file(path) result(bytes)
        character(*), intent(in) :: path
        integer(int8), allocatable :: bytes(:)
        integer :: u, n
        open (newunit=u, file=path, access="stream", form="unformatted", status="old", &
            action="read")
        inquire (unit=u, size=n)
        allocate (bytes(n))
        read (u) bytes
        close (u)
    end function read_file

end program test_fortfont
