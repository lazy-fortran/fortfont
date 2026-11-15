module fortfont_sfnt
    !! The tables of an sfnt font (TrueType, OpenType bitmap, collections) that
    !! text rendering needs: character map, metrics, names, outlines and
    !! embedded bitmap strikes (EBLC/EBDT monochrome or grey, CBLC/CBDT colour).
    use iso_fortran_env, only: int8, int32
    use fortfont_tt_binary, only: tt_byte, tt_ushort, tt_short, tt_ulong, tt_tag_match
    use fortfont_tt_tables, only: tt_find_table, tt_get_font_offset_for_index
    use fortfont_tt_cmap, only: tt_find_glyph_index
    implicit none
    private
    public :: sfnt_t, strike_t, sbit_t, sfnt_open, sfnt_glyph, sfnt_advance, sfnt_faces, &
        sfnt_bitmap, sfnt_kerning

    type :: strike_t
        !! One embedded bitmap size.
        integer :: ppem = 0, depth = 0, first = 0, last = 0, array = 0, count = 0
        integer :: ascender = 0, descender = 0
        logical :: colour = .false.
    end type strike_t

    type :: sbit_t
        !! One embedded bitmap glyph: alpha (or RGBA when colour) and metrics in
        !! pixels; top is the distance from the baseline up to the first row.
        integer :: width = 0, height = 0, left = 0, top = 0, advance = 0
        logical :: colour = .false.
        integer(int8), allocatable :: alpha(:, :) ! (width, height)
        integer(int8), allocatable :: png(:) ! colour glyphs: a PNG file
    end type sbit_t

    type :: sfnt_t
        integer(int8), allocatable :: data(:)
        integer :: start = 0, cmap = 0, head = 0, hhea = 0, hmtx = 0, loca = 0, glyf = 0
        integer :: maxp = 0, os2 = 0, name = 0, kern = 0, ebdt = 0, cbdt = 0
        integer :: num_glyphs = 0, units_per_em = 1000, loc_format = 0, num_hmetrics = 0
        integer :: ascender = 0, descender = 0, line_gap = 0
        integer :: weight = 400, x_height = 0, cap_height = 0
        logical :: italic = .false., outlines = .false.
        character(len=:), allocatable :: family, style
        type(strike_t), allocatable :: strikes(:)
    end type sfnt_t

contains

    integer function sfnt_faces(data) result(n)
        !! Number of fonts in the file: the count of a collection, else 1 or 0.
        integer(int8), intent(in) :: data(:)
        n = 0
        if (size(data) < 12) return
        if (tt_tag_match(data, 0, "ttcf")) then
            n = tt_ulong(data, 8)
        else if (tt_get_font_offset_for_index(data, 0) >= 0) then
            n = 1
        else if (tt_ulong(data, 0) == int(z'00010000') .or. tt_tag_match(data, 0, "true") &
                .or. tt_tag_match(data, 0, "OTTO")) then
            n = 1
        end if
    end function sfnt_faces

    logical function sfnt_open(font, data, face) result(ok)
        !! The face-th font (from 0) of a font file held in `data`.
        type(sfnt_t), intent(out) :: font
        integer(int8), intent(in) :: data(:)
        integer, intent(in), optional :: face
        integer :: index, k, enc, n, fmt, platform, encoding, off, flags
        ok = .false.
        index = 0
        if (present(face)) index = face
        font%data = data
        if (size(data) < 12) return
        if (tt_tag_match(data, 0, "ttcf")) then
            if (index >= tt_ulong(data, 8)) return
            font%start = tt_ulong(data, 12 + 4 * index)
        else
            if (index /= 0) return
            font%start = 0
        end if
        if (font%start < 0 .or. font%start + 12 > size(data)) return
        font%cmap = table("cmap")
        font%head = table("head")
        font%hhea = table("hhea")
        font%hmtx = table("hmtx")
        font%loca = table("loca")
        font%glyf = table("glyf")
        font%maxp = table("maxp")
        font%os2 = table("OS/2")
        font%name = table("name")
        font%kern = table("kern")
        if (font%head == 0 .or. font%maxp == 0 .or. font%cmap == 0) return
        font%units_per_em = tt_ushort(data, font%head + 18)
        font%loc_format = tt_short(data, font%head + 50)
        font%num_glyphs = tt_ushort(data, font%maxp + 4)
        font%outlines = font%glyf > 0 .and. font%loca > 0
        if (font%hhea > 0) then
            font%ascender = tt_short(data, font%hhea + 4)
            font%descender = tt_short(data, font%hhea + 6)
            font%line_gap = tt_short(data, font%hhea + 8)
            font%num_hmetrics = tt_ushort(data, font%hhea + 34)
        end if
        if (font%os2 > 0) then
            font%weight = tt_ushort(data, font%os2 + 4)
            flags = tt_ushort(data, font%os2 + 62)
            font%italic = iand(flags, 1) /= 0
            ! USE_TYPO_METRICS, or hhea without metrics: the typographic ones.
            if (iand(flags, 128) /= 0 .or. (font%ascender == 0 .and. font%descender == 0)) then
                font%ascender = tt_short(data, font%os2 + 68)
                font%descender = tt_short(data, font%os2 + 70)
                font%line_gap = tt_short(data, font%os2 + 72)
            end if
            if (tt_ushort(data, font%os2) >= 2) then
                font%x_height = tt_short(data, font%os2 + 86)
                font%cap_height = tt_short(data, font%os2 + 88)
            end if
        end if
        ! A Unicode character map: format 4 or 12 of platform 3 (1 or 10) or 0.
        n = tt_ushort(data, font%cmap + 2)
        enc = 0
        do k = 0, n - 1
            platform = tt_ushort(data, font%cmap + 4 + 8 * k)
            encoding = tt_ushort(data, font%cmap + 6 + 8 * k)
            off = font%cmap + tt_ulong(data, font%cmap + 8 + 8 * k)
            fmt = tt_ushort(data, off)
            if (platform == 3 .and. encoding == 10 .and. fmt == 12) then
                enc = off
                exit
            end if
            if ((platform == 3 .and. encoding == 1) .or. platform == 0) then
                if (enc == 0 .or. fmt == 12) enc = off
            end if
        end do
        if (enc == 0) return
        font%cmap = enc
        call read_names(font)
        call read_strikes(font, "EBLC", "EBDT", .false.)
        call read_strikes(font, "CBLC", "CBDT", .true.)
        ok = font%outlines .or. allocated(font%strikes)
    contains
        integer function table(tag)
            character(4), intent(in) :: tag
            table = tt_find_table(data, font%start, tag)
            if (table < 0) table = 0
        end function table
    end function sfnt_open

    integer function sfnt_glyph(font, codepoint) result(g)
        !! The glyph of a Unicode code point, 0 when the font has none.
        type(sfnt_t), intent(in) :: font
        integer, intent(in) :: codepoint
        g = tt_find_glyph_index(font%data, font%cmap, codepoint)
    end function sfnt_glyph

    subroutine sfnt_advance(font, g, advance, lsb)
        !! Advance width and left side bearing of glyph g in font units.
        type(sfnt_t), intent(in) :: font
        integer, intent(in) :: g
        integer, intent(out) :: advance, lsb
        integer :: n
        advance = 0
        lsb = 0
        if (font%hmtx == 0 .or. font%num_hmetrics == 0) return
        n = font%num_hmetrics
        if (g < n) then
            advance = tt_ushort(font%data, font%hmtx + 4 * g)
            lsb = tt_short(font%data, font%hmtx + 4 * g + 2)
        else
            advance = tt_ushort(font%data, font%hmtx + 4 * (n - 1))
            lsb = tt_short(font%data, font%hmtx + 4 * n + 2 * (g - n))
        end if
    end subroutine sfnt_advance

    integer function sfnt_kerning(font, left, right) result(k)
        !! The kern table's format 0 adjustment between two glyphs, font units.
        type(sfnt_t), intent(in) :: font
        integer, intent(in) :: left, right
        integer :: ntables, off, t, length, coverage, npairs, lo, hi, mid, key, here
        k = 0
        if (font%kern == 0) return
        if (tt_ushort(font%data, font%kern) /= 0) return
        ntables = tt_ushort(font%data, font%kern + 2)
        off = font%kern + 4
        key = left * 65536 + right
        do t = 1, ntables
            length = tt_ushort(font%data, off + 2)
            coverage = tt_ushort(font%data, off + 4)
            ! Horizontal kerning values in format 0 only.
            if (iand(coverage, 1) /= 0 .and. ishft(coverage, -8) == 0) then
                npairs = tt_ushort(font%data, off + 6)
                lo = 0
                hi = npairs - 1
                do while (lo <= hi)
                    mid = (lo + hi) / 2
                    here = tt_ushort(font%data, off + 14 + 6 * mid) * 65536 + &
                        tt_ushort(font%data, off + 16 + 6 * mid)
                    if (here == key) then
                        k = k + tt_short(font%data, off + 18 + 6 * mid)
                        exit
                    else if (here < key) then
                        lo = mid + 1
                    else
                        hi = mid - 1
                    end if
                end do
            end if
            off = off + length
        end do
    end function sfnt_kerning

    subroutine read_names(font)
        !! Family and style: typographic names (16, 17) before 1 and 2, the
        !! Windows English or any Unicode record before Macintosh Roman.
        type(sfnt_t), intent(inout) :: font
        font%family = name_string(font, 16)
        if (len(font%family) == 0) font%family = name_string(font, 1)
        font%style = name_string(font, 17)
        if (len(font%style) == 0) font%style = name_string(font, 2)
    end subroutine read_names

    function name_string(font, id) result(s)
        type(sfnt_t), intent(in) :: font
        integer, intent(in) :: id
        character(len=:), allocatable :: s
        integer :: n, k, rec, platform, encoding, language, nameid, length, offset, strings
        integer :: best, best_rank, rank, j, c
        s = ""
        if (font%name == 0) return
        n = tt_ushort(font%data, font%name + 2)
        strings = font%name + tt_ushort(font%data, font%name + 4)
        best = -1
        best_rank = 99
        do k = 0, n - 1
            rec = font%name + 6 + 12 * k
            platform = tt_ushort(font%data, rec)
            encoding = tt_ushort(font%data, rec + 2)
            language = tt_ushort(font%data, rec + 4)
            nameid = tt_ushort(font%data, rec + 6)
            if (nameid /= id) cycle
            rank = 99
            if (platform == 3 .and. (encoding == 1 .or. encoding == 10)) then
                rank = merge(0, 1, language == 1033)
            else if (platform == 0) then
                rank = 2
            else if (platform == 1 .and. encoding == 0) then
                rank = 3
            end if
            if (rank < best_rank) then
                best_rank = rank
                best = rec
            end if
        end do
        if (best < 0) return
        platform = tt_ushort(font%data, best)
        length = tt_ushort(font%data, best + 8)
        offset = strings + tt_ushort(font%data, best + 10)
        if (offset + length > size(font%data)) return
        if (platform == 1) then
            s = repeat(" ", length)
            do j = 1, length
                s(j:j) = achar(tt_byte(font%data, offset + j - 1))
            end do
        else
            ! UTF-16BE; characters outside ASCII become '?', which names rarely need.
            s = repeat(" ", length / 2)
            do j = 1, length / 2
                c = tt_ushort(font%data, offset + 2 * (j - 1))
                if (c < 128) then
                    s(j:j) = achar(c)
                else
                    s(j:j) = "?"
                end if
            end do
        end if
    end function name_string

    subroutine read_strikes(font, loc_tag, data_tag, colour)
        type(sfnt_t), intent(inout) :: font
        character(4), intent(in) :: loc_tag, data_tag
        logical, intent(in) :: colour
        integer :: loc, dat, n, k, rec
        type(strike_t), allocatable :: all(:)
        loc = tt_find_table(font%data, font%start, loc_tag)
        dat = tt_find_table(font%data, font%start, data_tag)
        if (loc <= 0 .or. dat <= 0) return
        if (colour) then
            font%cbdt = dat
        else
            font%ebdt = dat
        end if
        n = tt_ulong(font%data, loc + 4)
        if (n <= 0 .or. n > 256) return
        allocate (all(n))
        do k = 1, n
            rec = loc + 8 + 48 * (k - 1)
            all(k)%array = loc + tt_ulong(font%data, rec)
            all(k)%count = tt_ulong(font%data, rec + 8)
            all(k)%ascender = s8(tt_byte(font%data, rec + 16))
            all(k)%descender = s8(tt_byte(font%data, rec + 17))
            all(k)%first = tt_ushort(font%data, rec + 40)
            all(k)%last = tt_ushort(font%data, rec + 42)
            all(k)%ppem = tt_byte(font%data, rec + 45)
            all(k)%depth = tt_byte(font%data, rec + 46)
            all(k)%colour = colour
        end do
        if (allocated(font%strikes)) then
            font%strikes = [font%strikes, all]
        else
            call move_alloc(all, font%strikes)
        end if
    end subroutine read_strikes

    logical function sfnt_bitmap(font, strike, g, out) result(ok)
        !! The embedded bitmap of glyph g in a strike.
        type(sfnt_t), intent(in) :: font
        type(strike_t), intent(in) :: strike
        integer, intent(in) :: g
        type(sbit_t), intent(out) :: out
        integer :: k, entry, first, last, sub, index_fmt, image_fmt, image_data, start, finish
        integer :: n, lo, hi, mid, gid, image_size, base, dat
        integer :: big(8)
        logical :: have_big
        ok = .false.
        have_big = .false.
        dat = merge(font%cbdt, font%ebdt, strike%colour)
        if (g < strike%first .or. g > strike%last) return
        do k = 0, strike%count - 1
            entry = strike%array + 8 * k
            first = tt_ushort(font%data, entry)
            last = tt_ushort(font%data, entry + 2)
            if (g < first .or. g > last) cycle
            sub = strike%array + tt_ulong(font%data, entry + 4)
            index_fmt = tt_ushort(font%data, sub)
            image_fmt = tt_ushort(font%data, sub + 2)
            image_data = dat + tt_ulong(font%data, sub + 4)
            select case (index_fmt)
            case (1)
                start = tt_ulong(font%data, sub + 8 + 4 * (g - first))
                finish = tt_ulong(font%data, sub + 12 + 4 * (g - first))
            case (3)
                start = tt_ushort(font%data, sub + 8 + 2 * (g - first))
                finish = tt_ushort(font%data, sub + 10 + 2 * (g - first))
            case (2)
                image_size = tt_ulong(font%data, sub + 8)
                big = metrics8(sub + 12)
                have_big = .true.
                start = (g - first) * image_size
                finish = start + image_size
            case (4)
                n = tt_ulong(font%data, sub + 8)
                start = -1
                do lo = 0, n - 1
                    gid = tt_ushort(font%data, sub + 12 + 4 * lo)
                    if (gid == g) then
                        start = tt_ushort(font%data, sub + 14 + 4 * lo)
                        finish = tt_ushort(font%data, sub + 18 + 4 * lo)
                        exit
                    end if
                end do
                if (start < 0) return
            case (5)
                image_size = tt_ulong(font%data, sub + 8)
                big = metrics8(sub + 12)
                have_big = .true.
                n = tt_ulong(font%data, sub + 20)
                lo = 0
                hi = n - 1
                start = -1
                do while (lo <= hi)
                    mid = (lo + hi) / 2
                    gid = tt_ushort(font%data, sub + 24 + 2 * mid)
                    if (gid == g) then
                        start = mid * image_size
                        exit
                    else if (gid < g) then
                        lo = mid + 1
                    else
                        hi = mid - 1
                    end if
                end do
                if (start < 0) return
                finish = start + image_size
            case default
                return
            end select
            if (finish <= start) return
            base = image_data + start
            ok = decode_image(base, image_data + finish)
            return
        end do
    contains
        function metrics8(at) result(m)
            !! BigGlyphMetrics: height, width, bearing x, bearing y, advance, ...
            integer, intent(in) :: at
            integer :: m(8)
            m = [tt_byte(font%data, at), tt_byte(font%data, at + 1), &
                s8(tt_byte(font%data, at + 2)), s8(tt_byte(font%data, at + 3)), &
                tt_byte(font%data, at + 4), s8(tt_byte(font%data, at + 5)), &
                s8(tt_byte(font%data, at + 6)), tt_byte(font%data, at + 7)]
        end function metrics8

        logical function decode_image(at, finish) result(good)
            integer, intent(in) :: at, finish
            integer :: p, h, w, bx, by, adv, length
            good = .false.
            p = at
            select case (image_fmt)
            case (1, 2, 8, 17)
                h = tt_byte(font%data, p)
                w = tt_byte(font%data, p + 1)
                bx = s8(tt_byte(font%data, p + 2))
                by = s8(tt_byte(font%data, p + 3))
                adv = tt_byte(font%data, p + 4)
                p = p + 5
            case (6, 7, 9, 18)
                h = tt_byte(font%data, p)
                w = tt_byte(font%data, p + 1)
                bx = s8(tt_byte(font%data, p + 2))
                by = s8(tt_byte(font%data, p + 3))
                adv = tt_byte(font%data, p + 4)
                p = p + 8
            case (5, 19)
                if (.not. have_big) return
                h = big(1)
                w = big(2)
                bx = big(3)
                by = big(4)
                adv = big(5)
            case default
                return
            end select
            out%width = w
            out%height = h
            out%left = bx
            out%top = by
            out%advance = adv
            select case (image_fmt)
            case (1, 6)
                good = unpack_bits(p, .true.)
            case (2, 5, 7)
                good = unpack_bits(p, .false.)
            case (17, 18, 19)
                length = tt_ulong(font%data, p)
                if (p + 4 + length > finish .or. p + 4 + length > size(font%data)) return
                out%colour = .true.
                out%png = font%data(p + 5:p + 4 + length)
                good = .true.
            case default
                good = .false.
            end select
        end function decode_image

        logical function unpack_bits(at, byte_aligned) result(good)
            !! Rows of `depth`-bit samples, each row on a byte boundary or not.
            integer, intent(in) :: at
            logical, intent(in) :: byte_aligned
            integer :: x, y, bit, depth, row_bits, v, maxv
            depth = max(1, strike%depth)
            maxv = 2**depth - 1
            row_bits = out%width * depth
            if (byte_aligned) row_bits = 8 * ((row_bits + 7) / 8)
            good = at + (row_bits * out%height + 7) / 8 <= size(font%data)
            if (.not. good) return
            allocate (out%alpha(out%width, out%height))
            do y = 0, out%height - 1
                do x = 0, out%width - 1
                    bit = y * row_bits + x * depth
                    v = iand(ishft(tt_byte(font%data, at + bit / 8), -(8 - depth - mod(bit, 8))), &
                        maxv)
                    out%alpha(x + 1, y + 1) = to_byte(v * 255 / maxv)
                end do
            end do
        end function unpack_bits
    end function sfnt_bitmap

    pure integer function s8(v)
        integer, intent(in) :: v
        s8 = v
        if (v > 127) s8 = v - 256
    end function s8

    pure integer(int8) function to_byte(v)
        integer, intent(in) :: v
        to_byte = int(iand(v, 255) - 256 * (iand(v, 255) / 128), int8)
    end function to_byte

end module fortfont_sfnt
