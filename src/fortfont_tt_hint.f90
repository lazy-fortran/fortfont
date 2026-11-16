module fortfont_tt_hint
    !! TrueType hinting: the font's instructions (fpgm, prep and the glyph
    !! programs) run as FreeType's default interpreter (version 40) runs them for
    !! grey-scale rendering. Moves along y act; moves along x are suppressed in
    !! backward-compatibility mode (the default unless the font opts out with
    !! INSTCTRL), so glyphs keep their exact horizontal shapes and widths; after
    !! both IUP instructions ran, nothing moves any more. Coordinates are 26.6
    !! fixed point, vectors 2.14, as in the TrueType specification.
    use, intrinsic :: iso_fortran_env, only: int8, int64, dp => real64
    use fortfont_tt_binary, only: tt_byte, tt_ushort, tt_short, tt_ulong, tt_tag_match
    use fortfont_tt_tables, only: tt_find_table
    use fortfont_tt_metrics, only: tt_get_glyf_offset
    use fortfont_tt_outlines, only: tt_vertex_t, TT_VMOVE, TT_VLINE, TT_VCURVE
    implicit none
    private
    public :: hinter_t, hinter_setup, hinter_size, hinter_glyph

    integer, parameter :: ik = int64
    integer, parameter :: PROG_FPGM = 1, PROG_PREP = 2, PROG_GLYPH = 3
    integer, parameter :: R_HALF = 0, R_GRID = 1, R_DOUBLE = 2, R_DOWN = 3, R_UP = 4, &
        R_OFF = 5, R_SUPER = 6, R_SUPER45 = 7
    integer, parameter :: MAX_CALL_DEPTH = 64
    integer(ik), parameter :: MAX_INSTRUCTIONS = 1000000_ik

    type :: gstate_t
        integer(ik) :: pv(2) = [16384_ik, 0_ik], fv(2) = [16384_ik, 0_ik], dv(2) = [16384_ik, 0_ik]
        integer :: rp0 = 0, rp1 = 0, rp2 = 0
        integer :: gep0 = 1, gep1 = 1, gep2 = 1
        integer(ik) :: loop = 1
        integer(ik) :: min_dist = 64, cvt_cutin = 68, sw_cutin = 0, sw_value = 0
        integer(ik) :: delta_base = 9, delta_shift = 3
        integer :: round_state = R_GRID
        integer(ik) :: period = 64, phase = 0, threshold = 32
        logical :: auto_flip = .true.
        integer :: instruct_control = 0
    end type gstate_t

    type :: zone_t
        integer :: n = 0
        integer(ik), allocatable :: orus(:, :), org(:, :), cur(:, :)
        logical, allocatable :: on(:), tx(:), ty(:)
        integer, allocatable :: ends(:)
    end type zone_t

    type :: fdef_t
        integer :: prog = 0, start = -1
    end type fdef_t

    type :: code_t
        integer(int8), allocatable :: b(:)
    end type code_t

    type :: hinter_t
        !! A font's programs and, after hinter_size, the state its prep leaves.
        logical :: present = .false., sized = .false.
        integer :: ppem = 0
        integer(ik) :: scale = 0
        type(code_t) :: fpgm, prep
        integer, allocatable :: cvt_units(:)
        integer :: max_stack = 0, max_storage = 0, max_fdefs = 0, max_twilight = 0
        integer(ik), allocatable :: cvt(:), storage(:)
        type(fdef_t), allocatable :: fdefs(:), idefs(:)
        type(zone_t) :: twilight
        type(gstate_t) :: gs
        integer(int8), allocatable :: data(:)
        integer :: loca = 0, glyf = 0, loc_format = 0, num_glyphs = 0, hmtx = 0, num_hmetrics = 0
        integer :: ascender = 0, descender = 0
        !! maxp's largest glyph program and the gasp ranges, which decide the
        !! symmetric smoothing GETINFO reports.
        integer :: max_instructions = 0, gasp_version = 0
        integer, allocatable :: gasp_ppem(:), gasp_flags(:)
    end type hinter_t

    type :: exec_t
        type(gstate_t) :: gs
        integer(ik), allocatable :: stack(:)
        integer :: sp = 0
        integer(ik), allocatable :: cvt(:), storage(:)
        type(fdef_t), allocatable :: fdefs(:), idefs(:)
        type(zone_t) :: z(0:1)
        type(code_t) :: progs(3)
        integer :: ppem = 0, range = 0, depth = 0
        integer(ik) :: scale = 0, count = 0
        !! FreeType's move vector (16.16) and which move it selects: 1 along x,
        !! 2 along y, 0 general.
        integer(ik) :: mv(2) = [65536_ik, 0_ik]
        integer :: move_kind = 1
        !! Backward compatibility: 4 while active, plus bits 1 and 2 once IUP
        !! ran along each axis (7 freezes the outline); 0 when off.
        integer :: bcm = 4
        !! Native ClearType along x: rounding along x is left out.
        logical :: native_x = .false., composite = .false.
        !! GETINFO's symmetric smoothing at this size.
        logical :: symmetric = .true.
        logical :: failed = .false.
    end type exec_t

contains

    ! ---------------------------------------------------------------- setup

    subroutine hinter_setup(h, data, start, loca, glyf, loc_format, num_glyphs, hmtx, &
            num_hmetrics, ascender, descender)
        !! The font's programs and tables; hinter_size runs fpgm and prep.
        type(hinter_t), intent(out) :: h
        integer(int8), intent(in) :: data(:)
        integer, intent(in) :: start, loca, glyf, loc_format, num_glyphs, hmtx, num_hmetrics
        integer, intent(in) :: ascender, descender
        integer :: off, n, k, maxp
        h%data = data
        h%loca = loca
        h%glyf = glyf
        h%loc_format = loc_format
        h%num_glyphs = num_glyphs
        h%hmtx = hmtx
        h%num_hmetrics = num_hmetrics
        h%ascender = ascender
        h%descender = descender
        call table_bytes("fpgm", h%fpgm%b)
        call table_bytes("prep", h%prep%b)
        off = tt_find_table(data, start, "cvt ")
        n = table_len("cvt ") / 2
        allocate (h%cvt_units(0:max(0, n) - 1))
        do k = 0, n - 1
            h%cvt_units(k) = tt_short(data, off + 2 * k)
        end do
        maxp = tt_find_table(data, start, "maxp")
        if (maxp > 0 .and. tt_ulong(data, maxp) == int(z'00010000')) then
            h%max_twilight = tt_ushort(data, maxp + 16)
            h%max_storage = tt_ushort(data, maxp + 18)
            h%max_fdefs = tt_ushort(data, maxp + 20)
            h%max_stack = tt_ushort(data, maxp + 24)
            h%max_instructions = tt_ushort(data, maxp + 26)
        end if
        off = tt_find_table(data, start, "gasp")
        n = 0
        if (off > 0 .and. table_len("gasp") >= 4) then
            h%gasp_version = tt_ushort(data, off)
            n = min(tt_ushort(data, off + 2), (table_len("gasp") - 4) / 4)
        end if
        allocate (h%gasp_ppem(n), h%gasp_flags(n))
        do k = 1, n
            h%gasp_ppem(k) = tt_ushort(data, off + 4 * k)
            h%gasp_flags(k) = tt_ushort(data, off + 4 * k + 2)
        end do
        ! FreeType hands fonts without fpgm and with at most 7 bytes of prep to
        ! its auto-hinter instead of the interpreter.
        h%present = size(h%fpgm%b) > 0 .or. size(h%prep%b) > 7
    contains
        subroutine table_bytes(tag, b)
            character(4), intent(in) :: tag
            integer(int8), allocatable, intent(out) :: b(:)
            integer :: o, l
            o = tt_find_table(data, start, tag)
            l = table_len(tag)
            if (o > 0 .and. l > 0) then
                b = data(o + 1:o + l)
            else
                allocate (b(0))
            end if
        end subroutine table_bytes
        integer function table_len(tag)
            character(4), intent(in) :: tag
            integer :: k2, nt
            table_len = 0
            nt = tt_ushort(data, start + 4)
            do k2 = 0, nt - 1
                if (tt_tag_match(data, start + 12 + 16 * k2, tag)) then
                    table_len = tt_ulong(data, start + 12 + 16 * k2 + 12)
                    return
                end if
            end do
        end function table_len
    end subroutine hinter_setup

    subroutine hinter_size(h, ppem, scale)
        !! Runs fpgm and prep at `ppem` pixels per em (`scale` 16.16 from font
        !! units to 26.6); the state they leave starts every glyph program.
        type(hinter_t), intent(inout) :: h
        integer, intent(in) :: ppem
        integer(ik), intent(in) :: scale
        type(exec_t) :: e
        integer :: k
        h%sized = .false.
        if (.not. h%present) return
        h%ppem = ppem
        h%scale = scale
        call new_exec(e, h)
        allocate (e%cvt(0:size(h%cvt_units) - 1))
        do k = 0, size(h%cvt_units) - 1
            e%cvt(k) = mul_fix(int(h%cvt_units(k), ik), scale)
        end do
        allocate (e%storage(0:max(h%max_storage, 1) - 1))
        e%storage = 0
        allocate (e%fdefs(0:max(h%max_fdefs, 1) - 1), e%idefs(0:255))
        e%progs(PROG_FPGM) = h%fpgm
        e%progs(PROG_PREP) = h%prep
        allocate (e%progs(PROG_GLYPH)%b(0))
        call zone_new(e%z(0), max(h%max_twilight, 0), 0)
        call zone_new(e%z(1), 0, 0)
        e%gs = gstate_t()
        e%range = PROG_FPGM
        e%bcm = 0
        call reset_run(e)
        call run_program(e, PROG_FPGM)
        if (e%failed) return
        e%gs = gstate_t()
        e%range = PROG_PREP
        call reset_run(e)
        call run_program(e, PROG_PREP)
        if (e%failed) return
        h%cvt = e%cvt
        h%storage = e%storage
        h%fdefs = e%fdefs
        h%idefs = e%idefs
        h%twilight = e%z(0)
        h%gs = gstate_t()
        h%gs%min_dist = e%gs%min_dist
        h%gs%cvt_cutin = e%gs%cvt_cutin
        h%gs%sw_cutin = e%gs%sw_cutin
        h%gs%sw_value = e%gs%sw_value
        h%gs%delta_base = e%gs%delta_base
        h%gs%delta_shift = e%gs%delta_shift
        h%gs%auto_flip = e%gs%auto_flip
        h%gs%instruct_control = e%gs%instruct_control
        h%sized = .true.
    end subroutine hinter_size

    subroutine new_exec(e, h)
        type(exec_t), intent(out) :: e
        type(hinter_t), intent(in) :: h
        allocate (e%stack(0:max(h%max_stack, 0) + 256 - 1))
        e%sp = 0
        e%ppem = h%ppem
        e%scale = h%scale
        e%symmetric = symmetric_smoothing(h)
    end subroutine new_exec

    logical function symmetric_smoothing(h) result(sym)
        !! FreeType's ClearType policy (DirectWrite's): hinted fonts smooth
        !! symmetrically above 20 ppem, unless a version-1 gasp range says
        !! otherwise.
        type(hinter_t), intent(in) :: h
        integer :: k
        sym = h%max_instructions == 0 .or. h%ppem > 20
        if (h%gasp_version < 1) return
        do k = 1, size(h%gasp_ppem)
            if (h%ppem <= h%gasp_ppem(k)) then
                sym = iand(h%gasp_flags(k), 8) /= 0
                return
            end if
        end do
    end function symmetric_smoothing

    subroutine reset_run(e)
        !! What FreeType resets before running prep or a glyph program.
        type(exec_t), intent(inout) :: e
        e%gs%pv = [16384_ik, 0_ik]
        e%gs%fv = e%gs%pv
        e%gs%dv = e%gs%pv
        e%gs%gep0 = 1
        e%gs%gep1 = 1
        e%gs%gep2 = 1
        e%gs%round_state = R_GRID
        e%gs%loop = 1
        e%sp = 0
        call set_moves(e)
    end subroutine reset_run

    subroutine set_moves(e)
        !! FreeType's Compute_Funcs: the move vector for the current freedom
        !! and projection vectors, and whether native ClearType applies along x.
        type(exec_t), intent(inout) :: e
        integer(ik) :: fdp
        fdp = shifta(e%gs%pv(1) * e%gs%fv(1) + e%gs%pv(2) * e%gs%fv(2) + 8192_ik, 14)
        if (fdp >= 16382) then
            e%mv = e%gs%fv * 4
        else if (abs(fdp) < 5461) then
            e%mv = 0
        else
            e%mv = e%gs%fv * 65536_ik / fdp
        end if
        e%move_kind = 0
        if (fdp >= 16382 .and. e%gs%fv(1) == 16384) then
            e%move_kind = 1
        else if (fdp >= 16382 .and. e%gs%fv(2) == 16384) then
            e%move_kind = 2
        end if
        e%native_x = e%bcm == 0 .and. e%gs%pv(2) == 0 .and. e%gs%fv(2) == 0 .and. &
            e%range == PROG_GLYPH
    end subroutine set_moves

    subroutine zone_new(z, n, nc)
        type(zone_t), intent(out) :: z
        integer, intent(in) :: n, nc
        z%n = n
        allocate (z%orus(2, 0:n - 1), z%org(2, 0:n - 1), z%cur(2, 0:n - 1))
        allocate (z%on(0:n - 1), z%tx(0:n - 1), z%ty(0:n - 1), z%ends(0:nc - 1))
        z%orus = 0
        z%org = 0
        z%cur = 0
        z%on = .true.
        z%tx = .false.
        z%ty = .false.
        z%ends = 0
    end subroutine zone_new

    ! ---------------------------------------------------------------- glyphs

    subroutine hinter_glyph(h, g, x_scale, v, nv, ok)
        !! Glyph g hinted, as vertices in 26.6 (move/line/curve), its origin
        !! where FreeType puts it (x shifted by the first phantom point, which
        !! the instructions move only outside backward compatibility).
        type(hinter_t), intent(in) :: h
        integer, intent(in) :: g
        integer(ik), intent(in) :: x_scale
        type(tt_vertex_t), allocatable, intent(out) :: v(:)
        integer, intent(out) :: nv
        logical, intent(out) :: ok
        type(zone_t) :: z
        integer(ik) :: pp(2, 4)
        integer :: k, ic, bcm
        ok = .false.
        nv = 0
        allocate (v(0))
        if (.not. h%sized) return
        ! prep may switch hinting off (bit 1) or ask for the default state (bit 2).
        if (iand(h%gs%instruct_control, 1) /= 0) return
        ic = h%gs%instruct_control
        if (iand(ic, 2) /= 0) ic = 0
        bcm = ieor(iand(ic, 4), 4)
        call load_hinted(h, g, x_scale, 0, bcm, z, pp, ok)
        if (.not. ok) return
        do k = 0, z%n - 1
            z%cur(1, k) = z%cur(1, k) - pp(1, 1)
        end do
        call zone_vertices(z, v, nv)
    end subroutine hinter_glyph

    recursive subroutine load_hinted(h, g, x_scale, depth, bcm, z, pp, ok)
        !! Glyph g hinted, a simple glyph by its own program, a composite from
        !! its hinted components and then its own; pp are the phantom points
        !! FreeType's loader keeps (scaled, and hinted only when backward
        !! compatibility is off).
        type(hinter_t), intent(in) :: h
        integer, intent(in) :: g, depth
        integer(ik), intent(in) :: x_scale
        integer, intent(inout) :: bcm
        type(zone_t), intent(out) :: z
        integer(ik), intent(out) :: pp(2, 4)
        logical, intent(out) :: ok
        integer :: off, nc, adv, lsb, xmin, ymax, k
        integer(int8), allocatable :: ins(:)
        integer(ik) :: my_pp(2, 4)
        logical :: use_my
        ok = .false.
        pp = 0
        if (depth > 8) return
        off = tt_get_glyf_offset(h%data, h%loca, h%glyf, h%loc_format, h%num_glyphs, g)
        call metrics(h, g, adv, lsb)
        if (off < 0) then
            call zone_new(z, 0, 0)
            ok = .true.
            return
        end if
        nc = tt_short(h%data, off)
        xmin = tt_short(h%data, off + 2)
        ymax = tt_short(h%data, off + 8)
        if (nc >= 0) then
            call load_simple(h, off, nc, z, ins)
            call set_phantoms(z, z%n - 4, xmin, adv, lsb, ymax, h, x_scale)
            call scale_zone(z, x_scale, h%scale)
            pp = z%cur(:, z%n - 4:z%n - 1)
            call hint_zone(h, z, ins, .false., bcm, pp)
            ok = .true.
        else
            call load_composite(h, off, x_scale, depth, bcm, z, ins, use_my, my_pp, ok)
            if (.not. ok) return
            call set_phantoms(z, z%n - 4, xmin, adv, lsb, ymax, h, x_scale)
            do k = 1, 4
                pp(1, k) = mul_fix(z%orus(1, z%n - 5 + k), x_scale)
                pp(2, k) = mul_fix(z%orus(2, z%n - 5 + k), h%scale)
            end do
            if (use_my) pp = my_pp
            ! The components' points are hinted already: the composite's own
            ! program sees them as its original outline.
            z%cur(:, z%n - 4:z%n - 1) = pp
            z%org = z%cur
            z%orus = z%cur
            if (size(ins) > 0 .and. z%n > 4) call hint_zone(h, z, ins, .true., bcm, pp)
        end if
        call drop_phantoms(z)
    end subroutine load_hinted

    subroutine metrics(h, g, adv, lsb)
        type(hinter_t), intent(in) :: h
        integer, intent(in) :: g
        integer, intent(out) :: adv, lsb
        integer :: n
        adv = 0
        lsb = 0
        if (h%hmtx == 0 .or. h%num_hmetrics == 0) return
        n = h%num_hmetrics
        if (g < n) then
            adv = tt_ushort(h%data, h%hmtx + 4 * g)
            lsb = tt_short(h%data, h%hmtx + 4 * g + 2)
        else
            adv = tt_ushort(h%data, h%hmtx + 4 * (n - 1))
            lsb = tt_short(h%data, h%hmtx + 4 * n + 2 * (g - n))
        end if
    end subroutine metrics

    subroutine load_simple(h, off, nc, z, ins)
        !! Points (font units), on-curve flags and contour ends of a simple
        !! glyph, four phantom points after them, and its instructions.
        type(hinter_t), intent(in) :: h
        integer, intent(in) :: off, nc
        type(zone_t), intent(out) :: z
        integer(int8), allocatable, intent(out) :: ins(:)
        integer :: k, n, p, ni, flags, rep, x, y, f
        integer, allocatable :: fl(:)
        n = 0
        if (nc > 0) n = tt_ushort(h%data, off + 10 + 2 * (nc - 1)) + 1
        call zone_new(z, n + 4, nc)
        do k = 0, nc - 1
            z%ends(k) = tt_ushort(h%data, off + 10 + 2 * k)
        end do
        p = off + 10 + 2 * nc
        ni = tt_ushort(h%data, p)
        ins = h%data(p + 3:p + 2 + ni)
        p = p + 2 + ni
        allocate (fl(0:max(n, 1) - 1))
        k = 0
        do while (k < n)
            flags = tt_byte(h%data, p)
            p = p + 1
            fl(k) = flags
            k = k + 1
            if (iand(flags, 8) /= 0) then
                rep = tt_byte(h%data, p)
                p = p + 1
                do while (rep > 0 .and. k < n)
                    fl(k) = flags
                    k = k + 1
                    rep = rep - 1
                end do
            end if
        end do
        x = 0
        do k = 0, n - 1
            f = fl(k)
            if (iand(f, 2) /= 0) then
                if (iand(f, 16) /= 0) then
                    x = x + tt_byte(h%data, p)
                else
                    x = x - tt_byte(h%data, p)
                end if
                p = p + 1
            else if (iand(f, 16) == 0) then
                x = x + tt_short(h%data, p)
                p = p + 2
            end if
            z%orus(1, k) = x
        end do
        y = 0
        do k = 0, n - 1
            f = fl(k)
            if (iand(f, 4) /= 0) then
                if (iand(f, 32) /= 0) then
                    y = y + tt_byte(h%data, p)
                else
                    y = y - tt_byte(h%data, p)
                end if
                p = p + 1
            else if (iand(f, 32) == 0) then
                y = y + tt_short(h%data, p)
                p = p + 2
            end if
            z%orus(2, k) = y
            z%on(k) = iand(f, 1) /= 0
        end do
    end subroutine load_simple

    recursive subroutine load_composite(h, off, x_scale, depth, bcm, z, ins, use_my, my_pp, ok)
        !! The hinted components, transformed and moved into place (y offsets
        !! rounded to the grid where the font asks), with four phantom points;
        !! the instructions when the last component announces them, and the
        !! phantom points of a component whose metrics the glyph uses.
        type(hinter_t), intent(in) :: h
        integer, intent(in) :: off, depth
        integer(ik), intent(in) :: x_scale
        integer, intent(inout) :: bcm
        type(zone_t), intent(out) :: z
        integer(int8), allocatable, intent(out) :: ins(:)
        logical, intent(out) :: use_my
        integer(ik), intent(out) :: my_pp(2, 4)
        logical, intent(out) :: ok
        type(zone_t) :: c, acc
        integer(ik) :: pp(2, 4), dx, dy, xx, yy, m(4)
        integer :: p, flags, gi, a1, a2, k, n0
        logical :: more, have_ins, scaled
        ok = .false.
        use_my = .false.
        my_pp = 0
        call zone_new(acc, 0, 0)
        p = off + 10
        more = .true.
        have_ins = .false.
        do while (more)
            flags = tt_ushort(h%data, p)
            gi = tt_ushort(h%data, p + 2)
            p = p + 4
            if (iand(flags, 1) /= 0) then
                if (iand(flags, 2) /= 0) then
                    a1 = tt_short(h%data, p)
                    a2 = tt_short(h%data, p + 2)
                else
                    a1 = tt_ushort(h%data, p)
                    a2 = tt_ushort(h%data, p + 2)
                end if
                p = p + 4
            else
                if (iand(flags, 2) /= 0) then
                    a1 = sbyte(tt_byte(h%data, p))
                    a2 = sbyte(tt_byte(h%data, p + 1))
                else
                    a1 = tt_byte(h%data, p)
                    a2 = tt_byte(h%data, p + 1)
                end if
                p = p + 2
            end if
            ! The transform in 16.16: xx, yx, xy, yy (x' = xx x + xy y).
            m = [65536_ik, 0_ik, 0_ik, 65536_ik]
            if (iand(flags, 8) /= 0) then
                m(1) = 4_ik * tt_short(h%data, p)
                m(4) = m(1)
                p = p + 2
            else if (iand(flags, 64) /= 0) then
                m(1) = 4_ik * tt_short(h%data, p)
                m(4) = 4_ik * tt_short(h%data, p + 2)
                p = p + 4
            else if (iand(flags, 128) /= 0) then
                m(1) = 4_ik * tt_short(h%data, p)
                m(2) = 4_ik * tt_short(h%data, p + 2)
                m(3) = 4_ik * tt_short(h%data, p + 4)
                m(4) = 4_ik * tt_short(h%data, p + 6)
                p = p + 8
            end if
            scaled = iand(flags, 8 + 64 + 128) /= 0
            more = iand(flags, 32) /= 0
            have_ins = iand(flags, 256) /= 0
            call load_hinted(h, gi, x_scale, depth + 1, bcm, c, pp, ok)
            if (.not. ok) return
            if (iand(flags, 512) /= 0) then
                use_my = .true.
                my_pp = pp
            end if
            if (scaled) then
                do k = 0, c%n - 1
                    xx = mul_fix(c%cur(1, k), m(1)) + mul_fix(c%cur(2, k), m(3))
                    yy = mul_fix(c%cur(1, k), m(2)) + mul_fix(c%cur(2, k), m(4))
                    c%cur(:, k) = [xx, yy]
                end do
            end if
            if (iand(flags, 2) /= 0) then
                dx = a1
                dy = a2
                if (scaled .and. iand(flags, 2048) /= 0) then
                    ! SCALED_COMPONENT_OFFSET: FreeType's guess at Apple's rule.
                    dx = mul_fix(dx, nint(hypot(real(m(1), dp), real(m(3), dp)), ik))
                    dy = mul_fix(dy, nint(hypot(real(m(4), dp), real(m(2), dp)), ik))
                end if
                dx = mul_fix(dx, x_scale)
                dy = mul_fix(dy, h%scale)
                ! ROUND_XY_TO_GRID: only y, as FreeType v40 rounds it (no hinting
                ! along x, so the horizontal offset stays exact).
                if (iand(flags, 4) /= 0) dy = pix_round(dy)
            else
                if (a1 < 0 .or. a1 >= acc%n .or. a2 < 0 .or. a2 >= c%n) return
                dx = acc%cur(1, a1) - c%cur(1, a2)
                dy = acc%cur(2, a1) - c%cur(2, a2)
            end if
            c%cur(1, :) = c%cur(1, :) + dx
            c%cur(2, :) = c%cur(2, :) + dy
            n0 = acc%n
            call append(acc, c)
        end do
        allocate (ins(0))
        if (have_ins) then
            k = tt_ushort(h%data, p)
            if (k > 0) ins = h%data(p + 3:p + 2 + k)
        end if
        call zone_new(z, acc%n + 4, size(acc%ends))
        z%cur(:, 0:acc%n - 1) = acc%cur
        z%org(:, 0:acc%n - 1) = acc%cur
        z%on(0:acc%n - 1) = acc%on
        z%ends = acc%ends
        ok = .true.
    end subroutine load_composite

    subroutine append(acc, c)
        type(zone_t), intent(inout) :: acc
        type(zone_t), intent(in) :: c
        type(zone_t) :: t
        integer :: n, nc
        n = acc%n + c%n
        nc = size(acc%ends) + size(c%ends)
        call zone_new(t, n, nc)
        if (acc%n > 0) then
            t%cur(:, 0:acc%n - 1) = acc%cur
            t%on(0:acc%n - 1) = acc%on
        end if
        if (c%n > 0) then
            t%cur(:, acc%n:n - 1) = c%cur
            t%on(acc%n:n - 1) = c%on
        end if
        if (size(acc%ends) > 0) t%ends(0:size(acc%ends) - 1) = acc%ends
        if (size(c%ends) > 0) t%ends(size(acc%ends):nc - 1) = c%ends + acc%n
        acc = t
    end subroutine append

    subroutine set_phantoms(z, first, xmin, adv, lsb, ymax, h, x_scale)
        !! FreeType's phantom points in font units: the horizontal origin and
        !! advance, the vertical origin and advance (from hhea without vmtx).
        type(zone_t), intent(inout) :: z
        integer, intent(in) :: first, xmin, adv, lsb, ymax
        type(hinter_t), intent(in) :: h
        integer(ik), intent(in) :: x_scale
        integer :: tsb, vadv
        tsb = h%ascender - ymax
        vadv = h%ascender - h%descender
        z%orus(:, first) = [int(xmin - lsb, ik), 0_ik]
        z%orus(:, first + 1) = [int(xmin - lsb + adv, ik), 0_ik]
        z%orus(:, first + 2) = [0_ik, int(ymax + tsb, ik)]
        z%orus(:, first + 3) = [0_ik, int(ymax + tsb - vadv, ik)]
        z%on(first:first + 3) = .true.
        if (x_scale < 0) continue
    end subroutine set_phantoms

    subroutine scale_zone(z, x_scale, y_scale)
        type(zone_t), intent(inout) :: z
        integer(ik), intent(in) :: x_scale, y_scale
        integer :: k
        do k = 0, z%n - 1
            z%org(1, k) = mul_fix(z%orus(1, k), x_scale)
            z%org(2, k) = mul_fix(z%orus(2, k), y_scale)
        end do
        z%cur = z%org
    end subroutine scale_zone

    subroutine drop_phantoms(z)
        type(zone_t), intent(inout) :: z
        type(zone_t) :: t
        integer :: n
        n = z%n - 4
        call zone_new(t, n, size(z%ends))
        if (n > 0) then
            t%cur = z%cur(:, 0:n - 1)
            t%org = z%org(:, 0:n - 1)
            t%orus = z%orus(:, 0:n - 1)
            t%on = z%on(0:n - 1)
        end if
        t%ends = z%ends
        z = t
    end subroutine drop_phantoms

    subroutine hint_zone(h, z, ins, composite, bcm, pp)
        !! FreeType's TT_Hint_Glyph: runs a glyph program on zone z (phantom
        !! points last, rounded first) from the state prep left. The loader's
        !! phantom points pp take the hinted ones only outside backward
        !! compatibility. A failing program leaves its moves so far, as
        !! FreeType's non-pedantic interpreter does.
        type(hinter_t), intent(in) :: h
        type(zone_t), intent(inout) :: z
        integer(int8), intent(in) :: ins(:)
        logical, intent(in) :: composite
        integer, intent(inout) :: bcm
        integer(ik), intent(inout) :: pp(2, 4)
        type(exec_t) :: e
        integer :: n
        n = z%n
        if (n >= 4) then
            z%cur(1, n - 4) = pix_round(z%cur(1, n - 4))
            z%cur(1, n - 3) = pix_round(z%cur(1, n - 3))
            z%cur(2, n - 2) = pix_round(z%cur(2, n - 2))
            z%cur(2, n - 1) = pix_round(z%cur(2, n - 1))
        end if
        if (size(ins) > 0) call run_glyph()
        if (bcm == 0 .and. n >= 4) pp = z%cur(:, n - 4:n - 1)
    contains
        subroutine run_glyph()
            call new_exec(e, h)
            e%cvt = h%cvt
            e%storage = h%storage
            e%fdefs = h%fdefs
            e%idefs = h%idefs
            e%z(0) = h%twilight
            e%z(1) = z
            if (iand(h%gs%instruct_control, 2) /= 0) then
                e%gs = gstate_t()
            else
                e%gs = h%gs
            end if
            e%progs(PROG_FPGM) = h%fpgm
            e%progs(PROG_PREP) = h%prep
            allocate (e%progs(PROG_GLYPH)%b, source=ins)
            e%bcm = iand(bcm, 4)
            e%composite = composite
            e%range = PROG_GLYPH
            if (composite) e%scale = 65536
            call reset_run(e)
            call run_program(e, PROG_GLYPH)
            z = e%z(1)
            bcm = e%bcm
        end subroutine run_glyph
    end subroutine hint_zone

    subroutine zone_vertices(z, v, nv)
        !! Contours of on- and off-curve points as move, line and quadratic
        !! curve vertices, implied on-curve points between two off-curve ones.
        type(zone_t), intent(in) :: z
        type(tt_vertex_t), allocatable, intent(inout) :: v(:)
        integer, intent(out) :: nv
        integer :: c, s, e, k, i, cnt
        integer(ik) :: sx, sy, cx, cy, mx, my
        logical :: prev_off
        deallocate (v)
        allocate (v(max(1, 3 * z%n + 2 * size(z%ends))))
        nv = 0
        s = 0
        do c = 0, size(z%ends) - 1
            e = z%ends(c)
            if (e < s .or. e >= z%n) exit
            if (z%on(s)) then
                sx = z%cur(1, s)
                sy = z%cur(2, s)
                k = s + 1
                cnt = e - s
            else if (z%on(e)) then
                sx = z%cur(1, e)
                sy = z%cur(2, e)
                k = s
                cnt = e - s
            else
                sx = half(z%cur(1, s) + z%cur(1, e))
                sy = half(z%cur(2, s) + z%cur(2, e))
                k = s
                cnt = e - s + 1
            end if
            call add(TT_VMOVE, sx, sy, 0_ik, 0_ik)
            prev_off = .false.
            cx = 0
            cy = 0
            do i = 0, cnt - 1
                if (z%on(k)) then
                    if (prev_off) then
                        call add(TT_VCURVE, z%cur(1, k), z%cur(2, k), cx, cy)
                    else
                        call add(TT_VLINE, z%cur(1, k), z%cur(2, k), 0_ik, 0_ik)
                    end if
                    prev_off = .false.
                else
                    if (prev_off) then
                        mx = half(cx + z%cur(1, k))
                        my = half(cy + z%cur(2, k))
                        call add(TT_VCURVE, mx, my, cx, cy)
                    end if
                    cx = z%cur(1, k)
                    cy = z%cur(2, k)
                    prev_off = .true.
                end if
                k = k + 1
            end do
            if (prev_off) then
                call add(TT_VCURVE, sx, sy, cx, cy)
            else
                call add(TT_VLINE, sx, sy, 0_ik, 0_ik)
            end if
            s = e + 1
        end do
    contains
        subroutine add(t, x, y, ax, ay)
            integer, intent(in) :: t
            integer(ik), intent(in) :: x, y, ax, ay
            nv = nv + 1
            v(nv)%vtype = t
            v(nv)%x = int(x)
            v(nv)%y = int(y)
            v(nv)%cx = int(ax)
            v(nv)%cy = int(ay)
        end subroutine add
        integer(ik) function half(a)
            !! As the C division of FreeType's outline decomposition.
            integer(ik), intent(in) :: a
            half = a / 2
        end function half
    end subroutine zone_vertices

    ! ---------------------------------------------------------------- interpreter

    recursive subroutine run_program(e, prog)
        type(exec_t), intent(inout) :: e
        integer, intent(in) :: prog
        call run(e, prog, 0, .false.)
    end subroutine run_program

    recursive subroutine run(e, prog, start, in_function)
        !! Executes from `start` to the end of the program, or to ENDF when
        !! running a function.
        type(exec_t), intent(inout) :: e
        integer, intent(in) :: prog, start
        logical, intent(in) :: in_function
        integer :: ip, op, n, k, nb
        integer(ik) :: a, b, c, d
        integer :: fn, cnt
        logical :: rot
        ip = start
        e%depth = e%depth + 1
        if (e%depth > MAX_CALL_DEPTH) then
            e%failed = .true.
        end if
        do while (.not. e%failed .and. ip < size(e%progs(prog)%b))
            e%count = e%count + 1
            if (e%count > MAX_INSTRUCTIONS) then
                e%failed = .true.
                exit
            end if
            op = iand(int(e%progs(prog)%b(ip + 1)), 255)
            nb = 1
            select case (op)
            case (int(z'40')) ! NPUSHB
                n = byte_at(ip + 1)
                do k = 1, n
                    call push(int(byte_at(ip + 1 + k), ik))
                end do
                nb = 2 + n
            case (int(z'41')) ! NPUSHW
                n = byte_at(ip + 1)
                do k = 1, n
                    call push(int(word_at(ip + 2 + 2 * (k - 1)), ik))
                end do
                nb = 2 + 2 * n
            case (int(z'B0'):int(z'B7')) ! PUSHB
                n = op - int(z'B0') + 1
                do k = 1, n
                    call push(int(byte_at(ip + k), ik))
                end do
                nb = 1 + n
            case (int(z'B8'):int(z'BF')) ! PUSHW
                n = op - int(z'B8') + 1
                do k = 1, n
                    call push(int(word_at(ip + 1 + 2 * (k - 1)), ik))
                end do
                nb = 1 + 2 * n
            case (int(z'00'):int(z'05')) ! SVTCA, SPVTCA, SFVTCA
                if (iand(op, 1) == 0) then
                    a = 0
                    b = 16384
                else
                    a = 16384
                    b = 0
                end if
                if (op < 2) then
                    e%gs%pv = [a, b]
                    e%gs%fv = [a, b]
                    e%gs%dv = [a, b]
                else if (op < 4) then
                    e%gs%pv = [a, b]
                    e%gs%dv = [a, b]
                else
                    e%gs%fv = [a, b]
                end if
                call compute_funcs()
            case (int(z'06'), int(z'07')) ! SPVTL
                b = pop()
                a = pop()
                rot = iand(op, 1) /= 0
                if (vec_to_line(e%gs%gep2, int(b), e%gs%gep1, int(a), rot, e%gs%pv, .false.)) then
                    e%gs%dv = e%gs%pv
                    call compute_funcs()
                end if
            case (int(z'08'), int(z'09')) ! SFVTL
                b = pop()
                a = pop()
                rot = iand(op, 1) /= 0
                if (vec_to_line(e%gs%gep2, int(b), e%gs%gep1, int(a), rot, e%gs%fv, .false.)) &
                    call compute_funcs()
            case (int(z'86'), int(z'87')) ! SDPVTL
                b = pop()
                a = pop()
                rot = iand(op, 1) /= 0
                if (vec_to_line(e%gs%gep2, int(b), e%gs%gep1, int(a), rot, e%gs%dv, .true.)) then
                    if (vec_to_line(e%gs%gep2, int(b), e%gs%gep1, int(a), rot, e%gs%pv, &
                        .false.)) call compute_funcs()
                end if
            case (int(z'0A')) ! SPVFS
                b = pop()
                a = pop()
                call normalize(sext16(a), sext16(b), e%gs%pv)
                e%gs%dv = e%gs%pv
                call compute_funcs()
            case (int(z'0B')) ! SFVFS
                b = pop()
                a = pop()
                call normalize(sext16(a), sext16(b), e%gs%fv)
                call compute_funcs()
            case (int(z'0C')) ! GPV
                call push(e%gs%pv(1))
                call push(e%gs%pv(2))
            case (int(z'0D')) ! GFV
                call push(e%gs%fv(1))
                call push(e%gs%fv(2))
            case (int(z'0E')) ! SFVTPV
                e%gs%fv = e%gs%pv
                call compute_funcs()
            case (int(z'0F')) ! ISECT
                call isect()
            case (int(z'10'))
                e%gs%rp0 = int(pop())
            case (int(z'11'))
                e%gs%rp1 = int(pop())
            case (int(z'12'))
                e%gs%rp2 = int(pop())
            case (int(z'13'))
                e%gs%gep0 = zone_arg(pop())
            case (int(z'14'))
                e%gs%gep1 = zone_arg(pop())
            case (int(z'15'))
                e%gs%gep2 = zone_arg(pop())
            case (int(z'16')) ! SZPS
                k = zone_arg(pop())
                e%gs%gep0 = k
                e%gs%gep1 = k
                e%gs%gep2 = k
            case (int(z'17')) ! SLOOP
                a = pop()
                if (a < 0) then
                    e%failed = .true.
                else
                    e%gs%loop = min(a, 65535_ik)
                end if
            case (int(z'18'))
                e%gs%round_state = R_GRID
            case (int(z'19'))
                e%gs%round_state = R_HALF
            case (int(z'3D'))
                e%gs%round_state = R_DOUBLE
            case (int(z'7D'))
                e%gs%round_state = R_DOWN
            case (int(z'7C'))
                e%gs%round_state = R_UP
            case (int(z'7A'))
                e%gs%round_state = R_OFF
            case (int(z'76')) ! SROUND
                call super_round(pop(), 16384_ik)
                e%gs%round_state = R_SUPER
            case (int(z'77')) ! S45ROUND
                call super_round(pop(), 11585_ik)
                e%gs%round_state = R_SUPER45
            case (int(z'1A')) ! SMD
                e%gs%min_dist = pop()
            case (int(z'1B')) ! ELSE: skip to the matching EIF
                ip = skip_to(ip, .false.)
                nb = 1
            case (int(z'1C')) ! JMPR
                a = pop()
                if (a == 0 .and. e%sp == 0) e%failed = .true.
                ip = ip + int(a)
                nb = 0
            case (int(z'1D'))
                e%gs%cvt_cutin = pop()
            case (int(z'1E'))
                e%gs%sw_cutin = pop()
            case (int(z'1F'))
                e%gs%sw_value = mul_fix(pop(), e%scale)
            case (int(z'20')) ! DUP
                a = pop()
                call push(a)
                call push(a)
            case (int(z'21'))
                a = pop()
            case (int(z'22'))
                e%sp = 0
            case (int(z'23')) ! SWAP
                b = pop()
                a = pop()
                call push(b)
                call push(a)
            case (int(z'24'))
                call push(int(e%sp, ik))
            case (int(z'25')) ! CINDEX
                a = pop()
                if (a <= 0 .or. a > e%sp) then
                    call push(0_ik)
                else
                    call push(e%stack(e%sp - a))
                end if
            case (int(z'26')) ! MINDEX
                a = pop()
                if (a > 0 .and. a <= e%sp) then
                    d = e%stack(e%sp - a)
                    e%stack(e%sp - a:e%sp - 2) = e%stack(e%sp - a + 1:e%sp - 1)
                    e%stack(e%sp - 1) = d
                end if
            case (int(z'8A')) ! ROLL
                c = pop()
                b = pop()
                a = pop()
                call push(b)
                call push(c)
                call push(a)
            case (int(z'27')) ! ALIGNPTS
                b = pop()
                a = pop()
                call alignpts(int(a), int(b))
            case (int(z'29')) ! UTP
                a = pop()
                call utp(int(a))
            case (int(z'2A')) ! LOOPCALL
                fn = int(pop())
                cnt = int(pop())
                do k = 1, cnt
                    if (e%failed) exit
                    call call_function(fn)
                end do
            case (int(z'2B')) ! CALL
                fn = int(pop())
                call call_function(fn)
            case (int(z'2C')) ! FDEF
                fn = int(pop())
                if (fn < 0 .or. fn > ubound(e%fdefs, 1)) call grow_fdefs_e(e, fn)
                if (.not. e%failed) then
                    e%fdefs(fn)%prog = prog
                    e%fdefs(fn)%start = ip + 1
                end if
                ip = skip_to_endf(ip)
                nb = 1
            case (int(z'2D')) ! ENDF
                if (in_function) exit
                e%failed = .true.
            case (int(z'89')) ! IDEF
                fn = int(pop())
                if (fn >= 0 .and. fn <= 255) then
                    e%idefs(fn)%prog = prog
                    e%idefs(fn)%start = ip + 1
                end if
                ip = skip_to_endf(ip)
                nb = 1
            case (int(z'2E'), int(z'2F')) ! MDAP
                call mdap(int(pop()), iand(op, 1) /= 0)
            case (int(z'3E'), int(z'3F')) ! MIAP
                b = pop()
                a = pop()
                call miap(int(a), int(b), iand(op, 1) /= 0)
            case (int(z'30'), int(z'31')) ! IUP
                call iup(iand(op, 1) /= 0)
            case (int(z'32'), int(z'33')) ! SHP
                call shp(iand(op, 1) /= 0)
            case (int(z'34'), int(z'35')) ! SHC
                call shc(int(pop()), iand(op, 1) /= 0)
            case (int(z'36'), int(z'37')) ! SHZ
                call shz(int(pop()), iand(op, 1) /= 0)
            case (int(z'38')) ! SHPIX
                call shpix()
            case (int(z'39')) ! IP
                call ip_points()
            case (int(z'3A'), int(z'3B')) ! MSIRP
                b = pop()
                a = pop()
                call msirp(int(a), b, iand(op, 1) /= 0)
            case (int(z'3C')) ! ALIGNRP
                call alignrp()
            case (int(z'42')) ! WS
                b = pop()
                a = pop()
                if (a >= 0 .and. a <= ubound(e%storage, 1)) e%storage(a) = b
            case (int(z'43')) ! RS
                a = pop()
                if (a < 0 .or. a > ubound(e%storage, 1)) then
                    call push(0_ik)
                else
                    call push(e%storage(a))
                end if
            case (int(z'44')) ! WCVTP
                b = pop()
                a = pop()
                call write_cvt(a, b)
            case (int(z'70')) ! WCVTF
                b = pop()
                a = pop()
                call write_cvt(a, mul_fix(b, e%scale))
            case (int(z'45')) ! RCVT
                a = pop()
                call push(read_cvt(a))
            case (int(z'46'), int(z'47')) ! GC
                a = pop()
                call gc(int(a), iand(op, 1) /= 0)
            case (int(z'48')) ! SCFS
                b = pop()
                a = pop()
                call scfs(int(a), b)
            case (int(z'49'), int(z'4A')) ! MD
                b = pop()
                a = pop()
                call push(md(int(a), int(b), iand(op, 1) /= 0))
            case (int(z'4B')) ! MPPEM
                call push(int(e%ppem, ik))
            case (int(z'4C')) ! MPS
                call push(int(e%ppem, ik))
            case (int(z'4D'))
                e%gs%auto_flip = .true.
            case (int(z'4E'))
                e%gs%auto_flip = .false.
            case (int(z'4F')) ! DEBUG
                a = pop()
            case (int(z'50'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a < b))
            case (int(z'51'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a <= b))
            case (int(z'52'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a > b))
            case (int(z'53'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a >= b))
            case (int(z'54'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a == b))
            case (int(z'55'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a /= b))
            case (int(z'56')) ! ODD
                a = pop()
                call push(merge(1_ik, 0_ik, iand(round_value(a, e%gs%round_state), 127_ik) == 64))
            case (int(z'57')) ! EVEN
                a = pop()
                call push(merge(1_ik, 0_ik, iand(round_value(a, e%gs%round_state), 127_ik) == 0))
            case (int(z'58')) ! IF
                a = pop()
                if (a == 0) then
                    ip = skip_to(ip, .true.)
                    nb = 1
                end if
            case (int(z'59')) ! EIF
                continue
            case (int(z'5A'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a /= 0 .and. b /= 0))
            case (int(z'5B'))
                b = pop()
                a = pop()
                call push(merge(1_ik, 0_ik, a /= 0 .or. b /= 0))
            case (int(z'5C'))
                a = pop()
                call push(merge(1_ik, 0_ik, a == 0))
            case (int(z'5D'), int(z'71'), int(z'72')) ! DELTAP1..3
                call deltap(op)
            case (int(z'73'), int(z'74'), int(z'75')) ! DELTAC1..3
                call deltac(op)
            case (int(z'5E'))
                e%gs%delta_base = pop()
            case (int(z'5F'))
                e%gs%delta_shift = pop()
                if (e%gs%delta_shift < 0 .or. e%gs%delta_shift > 6) e%failed = .true.
            case (int(z'60'))
                b = pop()
                a = pop()
                call push(a + b)
            case (int(z'61'))
                b = pop()
                a = pop()
                call push(a - b)
            case (int(z'62')) ! DIV
                b = pop()
                a = pop()
                if (b == 0) then
                    e%failed = .true.
                else
                    call push(muldiv_trunc(a, 64_ik, b))
                end if
            case (int(z'63')) ! MUL
                b = pop()
                a = pop()
                call push(muldiv(a, b, 64_ik))
            case (int(z'64'))
                call push(abs(pop()))
            case (int(z'65'))
                call push(-pop())
            case (int(z'66')) ! FLOOR
                call push(iand(pop(), -64_ik))
            case (int(z'67')) ! CEILING
                call push(iand(pop() + 63, -64_ik))
            case (int(z'68'):int(z'6B')) ! ROUND
                if (e%native_x) then
                    call push(pop())
                else
                    call push(round_value(pop(), e%gs%round_state))
                end if
            case (int(z'6C'):int(z'6F')) ! NROUND
                call push(pop())
            case (int(z'78')) ! JROT
                b = pop()
                a = pop()
                if (b /= 0) then
                    ip = ip + int(a)
                    nb = 0
                    if (a == 0 .and. e%sp == 0) e%failed = .true.
                end if
            case (int(z'79')) ! JROF
                b = pop()
                a = pop()
                if (b == 0) then
                    ip = ip + int(a)
                    nb = 0
                    if (a == 0 .and. e%sp == 0) e%failed = .true.
                end if
            case (int(z'7E')) ! SANGW
                a = pop()
            case (int(z'7F')) ! AA
                a = pop()
            case (int(z'80')) ! FLIPPT; like the range flips, not after both IUPs
                do while (e%gs%loop > 0 .and. .not. e%failed)
                    a = pop()
                    if (e%bcm /= 7 .and. pt_ok(1, int(a))) e%z(1)%on(a) = .not. e%z(1)%on(a)
                    e%gs%loop = e%gs%loop - 1
                end do
                e%gs%loop = 1
            case (int(z'81'), int(z'82')) ! FLIPRGON, FLIPRGOFF
                b = pop()
                a = pop()
                if (e%bcm /= 7 .and. pt_ok(1, int(a)) .and. pt_ok(1, int(b))) &
                    e%z(1)%on(a:b) = op == int(z'81')
            case (int(z'85')) ! SCANCTRL
                a = pop()
            case (int(z'8D')) ! SCANTYPE
                a = pop()
            case (int(z'8B'))
                b = pop()
                a = pop()
                call push(max(a, b))
            case (int(z'8C'))
                b = pop()
                a = pop()
                call push(min(a, b))
            case (int(z'8E')) ! INSTCTRL
                b = pop()
                a = pop()
                call instctrl(int(b), a)
            case (int(z'88')) ! GETINFO
                a = pop()
                d = 0
                if (iand(a, 1_ik) /= 0) d = 40
                if (iand(a, 64_ik) /= 0) d = ior(d, ishft(1_ik, 13))
                if (iand(a, 1024_ik) /= 0) d = ior(d, ishft(1_ik, 17))
                if (iand(a, 2048_ik) /= 0 .and. e%symmetric) d = ior(d, ishft(1_ik, 18))
                if (iand(a, 4096_ik) /= 0) d = ior(d, ishft(1_ik, 19))
                call push(d)
            case (int(z'C0'):int(z'DF')) ! MDRP
                call mdrp(op, int(pop()))
            case (int(z'E0'):int(z'FF')) ! MIRP
                c = pop()
                a = pop()
                call mirp(op, int(a), c)
            case default
                if (e%idefs(op)%start >= 0) then
                    call run(e, e%idefs(op)%prog, e%idefs(op)%start, .true.)
                else
                    e%failed = .true.
                end if
            end select
            ip = ip + nb
        end do
        e%depth = e%depth - 1
    contains
        integer function byte_at(p)
            integer, intent(in) :: p
            if (p >= size(e%progs(prog)%b)) then
                e%failed = .true.
                byte_at = 0
                return
            end if
            byte_at = iand(int(e%progs(prog)%b(p + 1)), 255)
        end function byte_at
        integer function word_at(p)
            integer, intent(in) :: p
            word_at = byte_at(p) * 256 + byte_at(p + 1)
            if (word_at >= 32768) word_at = word_at - 65536
        end function word_at
        integer function inslen(p)
            !! Length of the instruction at p, push data included.
            integer, intent(in) :: p
            integer :: o
            o = byte_at(p)
            if (o == int(z'40')) then
                inslen = 2 + byte_at(p + 1)
            else if (o == int(z'41')) then
                inslen = 2 + 2 * byte_at(p + 1)
            else if (o >= int(z'B0') .and. o <= int(z'B7')) then
                inslen = 1 + (o - int(z'B0') + 1)
            else if (o >= int(z'B8') .and. o <= int(z'BF')) then
                inslen = 1 + 2 * (o - int(z'B8') + 1)
            else
                inslen = 1
            end if
        end function inslen
        integer function skip_to(p, else_too) result(q)
            !! From IF (else_too) or ELSE at p to the matching ELSE or EIF.
            integer, intent(in) :: p
            logical, intent(in) :: else_too
            integer :: level, o
            level = 1
            q = p + inslen(p)
            do while (q < size(e%progs(prog)%b))
                o = byte_at(q)
                if (o == int(z'58')) then
                    level = level + 1
                else if (o == int(z'59')) then
                    level = level - 1
                    if (level == 0) return
                else if (o == int(z'1B') .and. level == 1 .and. else_too) then
                    return
                end if
                q = q + inslen(q)
            end do
            e%failed = .true.
        end function skip_to
        integer function skip_to_endf(p) result(q)
            integer, intent(in) :: p
            q = p + 1
            do while (q < size(e%progs(prog)%b))
                if (byte_at(q) == int(z'2D')) return
                q = q + inslen(q)
            end do
            e%failed = .true.
        end function skip_to_endf

        ! ---- stack

        subroutine push(x)
            integer(ik), intent(in) :: x
            if (e%sp > ubound(e%stack, 1)) then
                e%failed = .true.
                return
            end if
            e%stack(e%sp) = x
            e%sp = e%sp + 1
        end subroutine push

        integer(ik) function pop()
            ! A missing argument reads as zero, as in FreeType's non-pedantic mode.
            if (e%sp <= 0) then
                pop = 0
                return
            end if
            e%sp = e%sp - 1
            pop = e%stack(e%sp)
        end function pop

        ! ---- vectors and projections

        subroutine compute_funcs()
            call set_moves(e)
        end subroutine compute_funcs

        integer(ik) function project(dx, dy)
            integer(ik), intent(in) :: dx, dy
            project = dot14(dx, dy, e%gs%pv(1), e%gs%pv(2))
        end function project

        integer(ik) function dualproj(dx, dy)
            integer(ik), intent(in) :: dx, dy
            dualproj = dot14(dx, dy, e%gs%dv(1), e%gs%dv(2))
        end function dualproj

        logical function pt_ok(zn, p)
            integer, intent(in) :: zn, p
            pt_ok = p >= 0 .and. p < e%z(zn)%n
        end function pt_ok

        logical function vec_to_line(zb, b, za, a, rot, vec, use_org) result(ok)
            !! SPVTL, SFVTL, SDPVTL: the vector from point b (zone zb) to point a
            !! (zone za), turned by 90 degrees when rot; coincident points give
            !! the x axis and turn rotation off.
            integer, intent(in) :: zb, b, za, a
            logical, intent(inout) :: rot
            integer(ik), intent(inout) :: vec(2)
            logical, intent(in) :: use_org
            integer(ik) :: dx, dy, t
            ok = pt_ok(zb, b) .and. pt_ok(za, a)
            if (.not. ok) return
            if (use_org) then
                dx = e%z(za)%org(1, a) - e%z(zb)%org(1, b)
                dy = e%z(za)%org(2, a) - e%z(zb)%org(2, b)
            else
                dx = e%z(za)%cur(1, a) - e%z(zb)%cur(1, b)
                dy = e%z(za)%cur(2, a) - e%z(zb)%cur(2, b)
            end if
            if (dx == 0 .and. dy == 0) then
                dx = 16384
                rot = .false.
            end if
            if (rot) then
                t = dy
                dy = dx
                dx = -t
            end if
            call normalize(dx, dy, vec)
        end function vec_to_line

        ! ---- moves

        subroutine move(zn, p, dist)
            !! Moves point p along the freedom vector so that its projection
            !! grows by dist; x is held in backward-compatibility mode, y after
            !! both IUP instructions.
            integer, intent(in) :: zn, p
            integer(ik), intent(in) :: dist
            select case (e%move_kind)
            case (1)
                if (e%bcm == 0) e%z(zn)%cur(1, p) = e%z(zn)%cur(1, p) + dist
                e%z(zn)%tx(p) = .true.
            case (2)
                if (e%bcm /= 7) e%z(zn)%cur(2, p) = e%z(zn)%cur(2, p) + dist
                e%z(zn)%ty(p) = .true.
            case default
                ! FreeType lets x move with a horizontal projection (Comic Sans).
                if (e%mv(1) /= 0) then
                    if (e%bcm == 0 .or. e%gs%pv(2) == 0) &
                        e%z(zn)%cur(1, p) = e%z(zn)%cur(1, p) + mul_fix(dist, e%mv(1))
                    e%z(zn)%tx(p) = .true.
                end if
                if (e%mv(2) /= 0) then
                    if (e%bcm /= 7 .or. e%gs%pv(2) == 0) &
                        e%z(zn)%cur(2, p) = e%z(zn)%cur(2, p) + mul_fix(dist, e%mv(2))
                    e%z(zn)%ty(p) = .true.
                end if
            end select
        end subroutine move

        subroutine move_orig(zn, p, dist)
            integer, intent(in) :: zn, p
            integer(ik), intent(in) :: dist
            select case (e%move_kind)
            case (1)
                e%z(zn)%org(1, p) = e%z(zn)%org(1, p) + dist
            case (2)
                e%z(zn)%org(2, p) = e%z(zn)%org(2, p) + dist
            case default
                e%z(zn)%org(1, p) = e%z(zn)%org(1, p) + mul_fix(dist, e%mv(1))
                e%z(zn)%org(2, p) = e%z(zn)%org(2, p) + mul_fix(dist, e%mv(2))
            end select
        end subroutine move_orig

        subroutine move_zp2(p, dx, dy, touch)
            integer, intent(in) :: p
            integer(ik), intent(in) :: dx, dy
            logical, intent(in) :: touch
            integer :: zn
            zn = e%gs%gep2
            if (e%gs%fv(1) /= 0) then
                if (e%bcm == 0) e%z(zn)%cur(1, p) = e%z(zn)%cur(1, p) + dx
                if (touch) e%z(zn)%tx(p) = .true.
            end if
            if (e%gs%fv(2) /= 0) then
                if (e%bcm /= 7) e%z(zn)%cur(2, p) = e%z(zn)%cur(2, p) + dy
                if (touch) e%z(zn)%ty(p) = .true.
            end if
        end subroutine move_zp2

        ! ---- rounding

        integer(ik) function round_value(d, state) result(v)
            integer(ik), intent(in) :: d
            integer, intent(in) :: state
            integer(ik) :: per, ph, th
            per = e%gs%period
            ph = e%gs%phase
            th = e%gs%threshold
            select case (state)
            case (R_GRID)
                if (d >= 0) then
                    v = max(0_ik, iand(d + 32, -64_ik))
                else
                    v = min(0_ik, -iand(32 - d, -64_ik))
                end if
            case (R_HALF)
                if (d >= 0) then
                    v = iand(d, -64_ik) + 32
                    if (v < 0) v = 32
                else
                    v = -(iand(-d, -64_ik) + 32)
                    if (v > 0) v = -32
                end if
            case (R_DOUBLE)
                if (d >= 0) then
                    v = max(0_ik, iand(d + 16, -32_ik))
                else
                    v = min(0_ik, -iand(16 - d, -32_ik))
                end if
            case (R_DOWN)
                if (d >= 0) then
                    v = max(0_ik, iand(d, -64_ik))
                else
                    v = min(0_ik, -iand(-d, -64_ik))
                end if
            case (R_UP)
                if (d >= 0) then
                    v = max(0_ik, iand(d + 63, -64_ik))
                else
                    v = min(0_ik, -iand(63 - d, -64_ik))
                end if
            case (R_SUPER)
                if (d >= 0) then
                    v = iand(d + th - ph, -per) + ph
                    if (v < 0) v = ph
                else
                    v = -iand(th - ph - d, -per) - ph
                    if (v > 0) v = -ph
                end if
            case (R_SUPER45)
                if (d >= 0) then
                    v = ((d + th - ph) / per) * per + ph
                    if (v < 0) v = ph
                else
                    v = -(((th - ph - d) / per) * per) - ph
                    if (v > 0) v = -ph
                end if
            case default
                v = d
            end select
        end function round_value

        subroutine super_round(sel, grid)
            integer(ik), intent(in) :: sel, grid
            integer(ik) :: per, ph, th
            select case (int(iand(sel, 192_ik)))
            case (0)
                per = grid / 2
            case (128)
                per = grid * 2
            case default
                per = grid
            end select
            select case (int(iand(sel, 48_ik)))
            case (16)
                ph = per / 4
            case (32)
                ph = per / 2
            case (48)
                ph = per * 3 / 4
            case default
                ph = 0
            end select
            if (iand(sel, 15_ik) == 0) then
                th = per - 1
            else
                th = (iand(sel, 15_ik) - 4) * per / 8
            end if
            e%gs%period = shifta(per, 8)
            e%gs%phase = shifta(ph, 8)
            e%gs%threshold = shifta(th, 8)
        end subroutine super_round

        ! ---- cvt

        integer(ik) function read_cvt(i)
            integer(ik), intent(in) :: i
            read_cvt = 0
            if (i >= 0 .and. i <= ubound(e%cvt, 1)) read_cvt = e%cvt(i)
        end function read_cvt

        subroutine write_cvt(i, x)
            integer(ik), intent(in) :: i, x
            if (i >= 0 .and. i <= ubound(e%cvt, 1)) e%cvt(i) = x
        end subroutine write_cvt

        ! ---- functions

        recursive subroutine call_function(fn)
            integer, intent(in) :: fn
            if (fn < 0 .or. fn > ubound(e%fdefs, 1)) then
                e%failed = .true.
                return
            end if
            if (e%fdefs(fn)%start < 0) then
                e%failed = .true.
                return
            end if
            call run(e, e%fdefs(fn)%prog, e%fdefs(fn)%start, .true.)
        end subroutine call_function

        ! ---- point instructions

        subroutine mdap(p, rnd)
            integer, intent(in) :: p
            logical, intent(in) :: rnd
            integer(ik) :: dist, cur_dist
            integer :: zn
            zn = e%gs%gep0
            if (.not. pt_ok(zn, p)) return
            dist = 0
            if (rnd) then
                cur_dist = project(e%z(zn)%cur(1, p), e%z(zn)%cur(2, p))
                if (.not. e%native_x) dist = round_value(cur_dist, e%gs%round_state) - cur_dist
            end if
            call move(zn, p, dist)
            e%gs%rp0 = p
            e%gs%rp1 = p
        end subroutine mdap

        subroutine miap(p, ci, rnd)
            integer, intent(in) :: p, ci
            logical, intent(in) :: rnd
            integer(ik) :: dist, org_dist, cutin
            integer :: zn
            zn = e%gs%gep0
            e%gs%rp0 = p
            e%gs%rp1 = p
            if (.not. pt_ok(zn, p) .or. ci < 0 .or. ci > ubound(e%cvt, 1)) return
            dist = read_cvt(int(ci, ik))
            if (zn == 0) then
                e%z(zn)%org(:, p) = [mul_fix14(dist, e%gs%fv(1)), mul_fix14(dist, e%gs%fv(2))]
                e%z(zn)%cur(:, p) = e%z(zn)%org(:, p)
            end if
            org_dist = project(e%z(zn)%cur(1, p), e%z(zn)%cur(2, p))
            if (rnd) then
                cutin = e%gs%cvt_cutin
                if (e%native_x) cutin = shifta(cutin, 4)
                if (abs(dist - org_dist) > cutin) dist = org_dist
                if (.not. e%native_x) dist = round_value(dist, e%gs%round_state)
            end if
            call move(zn, p, dist - org_dist)
        end subroutine miap

        subroutine mdrp(opc, p)
            integer, intent(in) :: opc, p
            integer(ik) :: org_dist, dist, cur_dist
            integer :: z0, z1, r
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            r = e%gs%rp0
            if (.not. pt_ok(z1, p) .or. .not. pt_ok(z0, r)) then
                call relative_done(opc, p)
                return
            end if
            if (z0 == 0 .or. z1 == 0) then
                org_dist = dualproj(e%z(z1)%org(1, p) - e%z(z0)%org(1, r), &
                    e%z(z1)%org(2, p) - e%z(z0)%org(2, r))
            else
                org_dist = mul_fix(dualproj(e%z(z1)%orus(1, p) - e%z(z0)%orus(1, r), &
                    e%z(z1)%orus(2, p) - e%z(z0)%orus(2, r)), e%scale)
            end if
            if (e%gs%sw_cutin > 0 .and. org_dist < e%gs%sw_value + e%gs%sw_cutin .and. &
                org_dist > e%gs%sw_value - e%gs%sw_cutin) then
                if (org_dist >= 0) then
                    org_dist = e%gs%sw_value
                else
                    org_dist = -e%gs%sw_value
                end if
            end if
            if (iand(opc, 4) /= 0 .and. .not. e%native_x) then
                dist = round_value(org_dist, e%gs%round_state)
            else
                dist = org_dist
            end if
            if (iand(opc, 8) /= 0) dist = min_distance(org_dist, dist)
            cur_dist = project(e%z(z1)%cur(1, p) - e%z(z0)%cur(1, r), &
                e%z(z1)%cur(2, p) - e%z(z0)%cur(2, r))
            call move(z1, p, dist - cur_dist)
            call relative_done(opc, p)
        end subroutine mdrp

        subroutine relative_done(opc, p)
            !! MDRP and MIRP set the reference points even when they fail.
            integer, intent(in) :: opc, p
            e%gs%rp1 = e%gs%rp0
            e%gs%rp2 = p
            if (iand(opc, 16) /= 0) e%gs%rp0 = p
        end subroutine relative_done

        integer(ik) function min_distance(org_dist, dist) result(v)
            integer(ik), intent(in) :: org_dist, dist
            integer(ik) :: m
            m = e%gs%min_dist
            if (e%native_x) m = shifta(m, 1)
            v = dist
            if (org_dist >= 0) then
                if (v < m) v = m
            else
                if (v > -m) v = -m
            end if
        end function min_distance

        subroutine mirp(opc, p, ci)
            integer, intent(in) :: opc, p
            integer(ik), intent(in) :: ci
            integer(ik) :: cvt_dist, org_dist, cur_dist, dist, cutin
            integer :: z0, z1, r
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            r = e%gs%rp0
            if (.not. pt_ok(z1, p) .or. .not. pt_ok(z0, r) .or. ci < -1 .or. &
                ci > ubound(e%cvt, 1)) then
                call relative_done(opc, p)
                return
            end if
            if (ci < 0) then
                cvt_dist = 0
            else
                cvt_dist = e%cvt(ci)
            end if
            if (abs(cvt_dist - e%gs%sw_value) < e%gs%sw_cutin) then
                if (cvt_dist >= 0) then
                    cvt_dist = e%gs%sw_value
                else
                    cvt_dist = -e%gs%sw_value
                end if
            end if
            if (z1 == 0) then
                e%z(z1)%org(1, p) = e%z(z0)%org(1, r) + mul_fix14(cvt_dist, e%gs%fv(1))
                e%z(z1)%org(2, p) = e%z(z0)%org(2, r) + mul_fix14(cvt_dist, e%gs%fv(2))
                e%z(z1)%cur(:, p) = e%z(z1)%org(:, p)
            end if
            org_dist = dualproj(e%z(z1)%org(1, p) - e%z(z0)%org(1, r), &
                e%z(z1)%org(2, p) - e%z(z0)%org(2, r))
            cur_dist = project(e%z(z1)%cur(1, p) - e%z(z0)%cur(1, r), &
                e%z(z1)%cur(2, p) - e%z(z0)%cur(2, r))
            if (e%gs%auto_flip) then
                if ((org_dist < 0) .neqv. (cvt_dist < 0)) cvt_dist = -cvt_dist
            end if
            dist = cvt_dist
            if (iand(opc, 4) /= 0) then
                if (z0 == z1) then
                    cutin = e%gs%cvt_cutin
                    if (e%native_x) cutin = shifta(cutin, 4)
                    if (abs(cvt_dist - org_dist) > cutin) cvt_dist = org_dist
                end if
                dist = cvt_dist
                if (.not. e%native_x) dist = round_value(cvt_dist, e%gs%round_state)
            end if
            if (iand(opc, 8) /= 0) dist = min_distance(org_dist, dist)
            call move(z1, p, dist - cur_dist)
            call relative_done(opc, p)
        end subroutine mirp

        subroutine msirp(p, d, set_rp0)
            integer, intent(in) :: p
            integer(ik), intent(in) :: d
            logical, intent(in) :: set_rp0
            integer(ik) :: dist
            integer :: z0, z1, r
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            r = e%gs%rp0
            if (.not. pt_ok(z1, p) .or. .not. pt_ok(z0, r)) return
            if (z1 == 0) then
                e%z(z1)%org(:, p) = e%z(z0)%org(:, r)
                call move_orig(z1, p, d)
                e%z(z1)%cur(:, p) = e%z(z1)%org(:, p)
            end if
            dist = project(e%z(z1)%cur(1, p) - e%z(z0)%cur(1, r), &
                e%z(z1)%cur(2, p) - e%z(z0)%cur(2, r))
            call move(z1, p, d - dist)
            e%gs%rp1 = e%gs%rp0
            e%gs%rp2 = p
            if (set_rp0) e%gs%rp0 = p
        end subroutine msirp

        subroutine alignrp()
            integer(ik) :: dist
            integer :: p, z0, z1, r
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            r = e%gs%rp0
            do while (e%gs%loop > 0 .and. .not. e%failed)
                p = int(pop())
                if (pt_ok(z1, p) .and. pt_ok(z0, r)) then
                    dist = project(e%z(z1)%cur(1, p) - e%z(z0)%cur(1, r), &
                        e%z(z1)%cur(2, p) - e%z(z0)%cur(2, r))
                    call move(z1, p, -dist)
                end if
                e%gs%loop = e%gs%loop - 1
            end do
            e%gs%loop = 1
        end subroutine alignrp

        subroutine alignpts(p1, p2)
            integer, intent(in) :: p1, p2
            integer(ik) :: dist
            if (.not. pt_ok(e%gs%gep1, p1) .or. .not. pt_ok(e%gs%gep0, p2)) return
            dist = project(e%z(e%gs%gep0)%cur(1, p2) - e%z(e%gs%gep1)%cur(1, p1), &
                e%z(e%gs%gep0)%cur(2, p2) - e%z(e%gs%gep1)%cur(2, p1)) / 2
            call move(e%gs%gep1, p1, dist)
            call move(e%gs%gep0, p2, -dist)
        end subroutine alignpts

        subroutine utp(p)
            integer, intent(in) :: p
            if (.not. pt_ok(e%gs%gep0, p)) return
            if (e%gs%fv(1) /= 0) e%z(e%gs%gep0)%tx(p) = .false.
            if (e%gs%fv(2) /= 0) e%z(e%gs%gep0)%ty(p) = .false.
        end subroutine utp

        subroutine gc(p, orig)
            integer, intent(in) :: p
            logical, intent(in) :: orig
            integer :: zn
            zn = e%gs%gep2
            if (.not. pt_ok(zn, p)) then
                call push(0_ik)
                return
            end if
            if (orig) then
                call push(dualproj(e%z(zn)%org(1, p), e%z(zn)%org(2, p)))
            else
                call push(project(e%z(zn)%cur(1, p), e%z(zn)%cur(2, p)))
            end if
        end subroutine gc

        subroutine scfs(p, val)
            integer, intent(in) :: p
            integer(ik), intent(in) :: val
            integer(ik) :: k
            integer :: zn
            zn = e%gs%gep2
            if (.not. pt_ok(zn, p)) return
            k = project(e%z(zn)%cur(1, p), e%z(zn)%cur(2, p))
            call move(zn, p, val - k)
            if (zn == 0) e%z(zn)%org(:, p) = e%z(zn)%cur(:, p)
        end subroutine scfs

        integer(ik) function md(a, b, current)
            integer, intent(in) :: a, b
            logical, intent(in) :: current
            integer :: z0, z1
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            md = 0
            if (.not. pt_ok(z0, a) .or. .not. pt_ok(z1, b)) return
            if (current) then
                md = project(e%z(z0)%cur(1, a) - e%z(z1)%cur(1, b), &
                    e%z(z0)%cur(2, a) - e%z(z1)%cur(2, b))
            else if (z0 == 0 .or. z1 == 0) then
                md = dualproj(e%z(z0)%org(1, a) - e%z(z1)%org(1, b), &
                    e%z(z0)%org(2, a) - e%z(z1)%org(2, b))
            else
                md = mul_fix(dualproj(e%z(z0)%orus(1, a) - e%z(z1)%orus(1, b), &
                    e%z(z0)%orus(2, a) - e%z(z1)%orus(2, b)), e%scale)
            end if
        end function md

        subroutine isect()
            integer :: p, a0, a1, b0, b1, z0, z1, z2
            integer(ik) :: dbx, dby, dax, day, dx, dy, disc, dotp, val
            b1 = int(pop())
            b0 = int(pop())
            a1 = int(pop())
            a0 = int(pop())
            p = int(pop())
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            z2 = e%gs%gep2
            if (.not. (pt_ok(z0, b0) .and. pt_ok(z0, b1) .and. pt_ok(z1, a0) .and. &
                pt_ok(z1, a1) .and. pt_ok(z2, p))) return
            dbx = e%z(z0)%cur(1, b1) - e%z(z0)%cur(1, b0)
            dby = e%z(z0)%cur(2, b1) - e%z(z0)%cur(2, b0)
            dax = e%z(z1)%cur(1, a1) - e%z(z1)%cur(1, a0)
            day = e%z(z1)%cur(2, a1) - e%z(z1)%cur(2, a0)
            dx = e%z(z0)%cur(1, b0) - e%z(z1)%cur(1, a0)
            dy = e%z(z0)%cur(2, b0) - e%z(z1)%cur(2, a0)
            disc = muldiv(dax, -dby, 64_ik) + muldiv(day, dbx, 64_ik)
            dotp = muldiv(dax, dbx, 64_ik) + muldiv(day, dby, 64_ik)
            if (19 * abs(disc) > abs(dotp)) then
                val = muldiv(dx, -dby, 64_ik) + muldiv(dy, dbx, 64_ik)
                e%z(z2)%cur(1, p) = e%z(z1)%cur(1, a0) + muldiv(val, dax, disc)
                e%z(z2)%cur(2, p) = e%z(z1)%cur(2, a0) + muldiv(val, day, disc)
            else
                e%z(z2)%cur(1, p) = (e%z(z1)%cur(1, a0) + e%z(z1)%cur(1, a1) + &
                    e%z(z0)%cur(1, b0) + e%z(z0)%cur(1, b1)) / 4
                e%z(z2)%cur(2, p) = (e%z(z1)%cur(2, a0) + e%z(z1)%cur(2, a1) + &
                    e%z(z0)%cur(2, b0) + e%z(z0)%cur(2, b1)) / 4
            end if
            e%z(z2)%tx(p) = .true.
            e%z(z2)%ty(p) = .true.
        end subroutine isect

        ! ---- shifts

        logical function displacement(use_rp1, dx, dy, zr, r)
            !! The move of the reference point (rp2 in zp1, or rp1 in zp0) along
            !! the projection vector, split along the freedom vector.
            logical, intent(in) :: use_rp1
            integer(ik), intent(out) :: dx, dy
            integer, intent(out) :: zr, r
            integer(ik) :: d
            if (use_rp1) then
                zr = e%gs%gep0
                r = e%gs%rp1
            else
                zr = e%gs%gep1
                r = e%gs%rp2
            end if
            dx = 0
            dy = 0
            displacement = pt_ok(zr, r)
            if (.not. displacement) return
            d = project(e%z(zr)%cur(1, r) - e%z(zr)%org(1, r), &
                e%z(zr)%cur(2, r) - e%z(zr)%org(2, r))
            dx = mul_fix(d, e%mv(1))
            dy = mul_fix(d, e%mv(2))
        end function displacement

        subroutine shp(use_rp1)
            logical, intent(in) :: use_rp1
            integer(ik) :: dx, dy
            integer :: zr, r, p
            logical :: ok
            ok = displacement(use_rp1, dx, dy, zr, r)
            do while (e%gs%loop > 0 .and. .not. e%failed)
                p = int(pop())
                if (ok .and. pt_ok(e%gs%gep2, p)) call move_zp2(p, dx, dy, .true.)
                e%gs%loop = e%gs%loop - 1
            end do
            e%gs%loop = 1
        end subroutine shp

        subroutine shc(c, use_rp1)
            integer, intent(in) :: c
            logical, intent(in) :: use_rp1
            integer(ik) :: dx, dy
            integer :: zr, r, i, s, l
            if (e%gs%gep2 == 0) then
                if (c /= 0) return
            else if (c < 0 .or. c >= size(e%z(1)%ends)) then
                return
            end if
            if (.not. displacement(use_rp1, dx, dy, zr, r)) return
            s = 0
            if (e%gs%gep2 == 0) then
                l = e%z(0)%n
            else
                if (c > 0) s = e%z(1)%ends(c - 1) + 1
                l = e%z(1)%ends(c) + 1
            end if
            do i = s, l - 1
                if (.not. pt_ok(e%gs%gep2, i)) exit
                if (zr /= e%gs%gep2 .or. r /= i) call move_zp2(i, dx, dy, .true.)
            end do
        end subroutine shc

        subroutine shz(zarg, use_rp1)
            integer, intent(in) :: zarg
            logical, intent(in) :: use_rp1
            integer(ik) :: dx, dy
            integer :: zr, r, i, l
            ! The zone is the argument's; phantom points stay; nothing is touched.
            if (zarg < 0 .or. zarg > 1) return
            if (.not. displacement(use_rp1, dx, dy, zr, r)) return
            if (zarg == 0) then
                l = e%z(0)%n
            else
                l = max(e%z(1)%n - 4, 0)
            end if
            do i = 0, l - 1
                if (zr == zarg .and. r == i) cycle
                if (dx /= 0 .and. e%bcm == 0) e%z(zarg)%cur(1, i) = e%z(zarg)%cur(1, i) + dx
                if (dy /= 0 .and. e%bcm /= 7) e%z(zarg)%cur(2, i) = e%z(zarg)%cur(2, i) + dy
            end do
        end subroutine shz

        subroutine shpix()
            integer(ik) :: amount, dx, dy
            integer :: p
            logical :: twilight
            amount = pop()
            dx = mul_fix14(amount, e%gs%fv(1))
            dy = mul_fix14(amount, e%gs%fv(2))
            twilight = e%gs%gep0 == 0 .or. e%gs%gep1 == 0 .or. e%gs%gep2 == 0
            do while (e%gs%loop > 0 .and. .not. e%failed)
                p = int(pop())
                if (pt_ok(e%gs%gep2, p)) then
                    if (e%bcm /= 0) then
                        if (twilight .or. (e%bcm /= 7 .and. &
                            ((e%composite .and. e%gs%fv(2) /= 0) .or. &
                            e%z(e%gs%gep2)%ty(p)))) call move_zp2(p, 0_ik, dy, .true.)
                    else
                        call move_zp2(p, dx, dy, .true.)
                    end if
                end if
                e%gs%loop = e%gs%loop - 1
            end do
            e%gs%loop = 1
        end subroutine shpix

        subroutine ip_points()
            integer(ik) :: base(2), cur_base(2), old_range, cur_range, org_dist, cur_dist, nd
            integer :: z0, z1, z2, p
            logical :: twilight
            z0 = e%gs%gep0
            z1 = e%gs%gep1
            z2 = e%gs%gep2
            twilight = z0 == 0 .or. z1 == 0 .or. z2 == 0
            old_range = 0
            cur_range = 0
            base = 0
            cur_base = 0
            if (.not. pt_ok(z0, e%gs%rp1)) then
                e%sp = max(e%sp - int(e%gs%loop), 0)
                e%gs%loop = 1
                return
            end if
            if (twilight) then
                base = e%z(z0)%org(:, e%gs%rp1)
            else
                base = e%z(z0)%orus(:, e%gs%rp1)
            end if
            cur_base = e%z(z0)%cur(:, e%gs%rp1)
            if (pt_ok(z1, e%gs%rp2)) then
                if (twilight) then
                    old_range = dualproj(e%z(z1)%org(1, e%gs%rp2) - base(1), &
                        e%z(z1)%org(2, e%gs%rp2) - base(2))
                else
                    old_range = dualproj(e%z(z1)%orus(1, e%gs%rp2) - base(1), &
                        e%z(z1)%orus(2, e%gs%rp2) - base(2))
                end if
                cur_range = project(e%z(z1)%cur(1, e%gs%rp2) - cur_base(1), &
                    e%z(z1)%cur(2, e%gs%rp2) - cur_base(2))
            end if
            do while (e%gs%loop > 0 .and. .not. e%failed)
                p = int(pop())
                if (pt_ok(z2, p)) then
                    if (twilight) then
                        org_dist = dualproj(e%z(z2)%org(1, p) - base(1), e%z(z2)%org(2, p) - base(2))
                    else
                        org_dist = dualproj(e%z(z2)%orus(1, p) - base(1), &
                            e%z(z2)%orus(2, p) - base(2))
                    end if
                    cur_dist = project(e%z(z2)%cur(1, p) - cur_base(1), &
                        e%z(z2)%cur(2, p) - cur_base(2))
                    if (org_dist /= 0) then
                        if (old_range /= 0) then
                            nd = muldiv(org_dist, cur_range, old_range)
                        else
                            nd = cur_dist
                        end if
                    else
                        nd = 0
                    end if
                    call move(z2, p, nd - cur_dist)
                end if
                e%gs%loop = e%gs%loop - 1
            end do
            e%gs%loop = 1
        end subroutine ip_points

        subroutine iup(xdir)
            !! Interpolates the untouched points of every contour between the
            !! touched ones (in font units), or shifts them with a lone one.
            logical, intent(in) :: xdir
            integer :: ax, c, s, ep, p, first_touched, cur_touched
            logical, allocatable :: t(:)
            if (e%bcm == 7) return
            if (e%bcm /= 0) e%bcm = ior(e%bcm, merge(2, 1, xdir))
            if (size(e%z(1)%ends) == 0) return
            ax = merge(1, 2, xdir)
            if (xdir) then
                t = e%z(1)%tx
            else
                t = e%z(1)%ty
            end if
            p = 0
            do c = 0, size(e%z(1)%ends) - 1
                ep = e%z(1)%ends(c)
                if (ep >= e%z(1)%n) ep = e%z(1)%n - 1
                s = p
                do while (p <= ep)
                    if (t(p)) exit
                    p = p + 1
                end do
                if (p <= ep) then
                    first_touched = p
                    cur_touched = p
                    p = p + 1
                    do while (p <= ep)
                        if (t(p)) then
                            call interp(ax, cur_touched + 1, p - 1, cur_touched, p)
                            cur_touched = p
                        end if
                        p = p + 1
                    end do
                    if (cur_touched == first_touched) then
                        call shift(ax, s, ep, cur_touched)
                    else
                        call interp(ax, cur_touched + 1, ep, cur_touched, first_touched)
                        if (first_touched > 0) call interp(ax, s, first_touched - 1, &
                            cur_touched, first_touched)
                    end if
                end if
                p = ep + 1
            end do
        end subroutine iup

        subroutine shift(ax, p1, p2, p)
            integer, intent(in) :: ax, p1, p2, p
            integer(ik) :: d
            integer :: i
            d = e%z(1)%cur(ax, p) - e%z(1)%org(ax, p)
            if (d == 0) return
            do i = p1, p2
                if (i /= p) e%z(1)%cur(ax, i) = e%z(1)%cur(ax, i) + d
            end do
        end subroutine shift

        subroutine interp(ax, p1, p2, r1_in, r2_in)
            integer, intent(in) :: ax, p1, p2, r1_in, r2_in
            integer :: r1, r2, i
            integer(ik) :: o1, o2, g1, g2, c1, c2, d1, d2, x, sc
            logical :: have
            if (p1 > p2) return
            r1 = r1_in
            r2 = r2_in
            if (r1 >= e%z(1)%n .or. r2 >= e%z(1)%n) return
            o1 = e%z(1)%orus(ax, r1)
            o2 = e%z(1)%orus(ax, r2)
            if (o1 > o2) then
                x = o1
                o1 = o2
                o2 = x
                i = r1
                r1 = r2
                r2 = i
            end if
            g1 = e%z(1)%org(ax, r1)
            g2 = e%z(1)%org(ax, r2)
            c1 = e%z(1)%cur(ax, r1)
            c2 = e%z(1)%cur(ax, r2)
            d1 = c1 - g1
            d2 = c2 - g2
            have = .false.
            sc = 0
            do i = p1, p2
                x = e%z(1)%org(ax, i)
                if (x <= g1) then
                    x = x + d1
                else if (x >= g2) then
                    x = x + d2
                else if (c1 == c2 .or. o1 == o2) then
                    x = c1
                else
                    if (.not. have) then
                        sc = div_fix(c2 - c1, o2 - o1)
                        have = .true.
                    end if
                    x = c1 + mul_fix(e%z(1)%orus(ax, i) - o1, sc)
                end if
                e%z(1)%cur(ax, i) = x
            end do
        end subroutine interp

        ! ---- deltas and control

        subroutine deltap(opc)
            integer, intent(in) :: opc
            integer(ik) :: n, a, b, c, k
            integer :: zn
            zn = e%gs%gep0
            n = pop()
            do k = 1, n
                if (e%failed) exit
                a = pop()
                b = pop()
                if (.not. pt_ok(zn, int(a))) cycle
                c = shifta(iand(b, 240_ik), 4)
                if (opc == int(z'71')) c = c + 16
                if (opc == int(z'72')) c = c + 32
                c = c + e%gs%delta_base
                if (c /= e%ppem) cycle
                b = iand(b, 15_ik) - 8
                if (b >= 0) b = b + 1
                b = b * 2_ik**(6 - e%gs%delta_shift)
                if (e%bcm /= 0) then
                    if (e%bcm /= 7 .and. ((e%composite .and. &
                        e%gs%fv(2) /= 0) .or. e%z(zn)%ty(a))) call move(zn, int(a), b)
                else
                    call move(zn, int(a), b)
                end if
            end do
        end subroutine deltap

        subroutine deltac(opc)
            integer, intent(in) :: opc
            integer(ik) :: n, a, b, c, k
            n = pop()
            do k = 1, n
                if (e%failed) exit
                a = pop()
                b = pop()
                c = shifta(iand(b, 240_ik), 4)
                if (opc == int(z'74')) c = c + 16
                if (opc == int(z'75')) c = c + 32
                c = c + e%gs%delta_base
                if (c /= e%ppem) cycle
                b = iand(b, 15_ik) - 8
                if (b >= 0) b = b + 1
                b = b * 2_ik**(6 - e%gs%delta_shift)
                call write_cvt(a, read_cvt(a) + b)
            end do
        end subroutine deltac

        subroutine instctrl(k, l)
            integer, intent(in) :: k
            integer(ik), intent(in) :: l
            integer :: kf, lv
            if (k < 1 .or. k > 3) return
            kf = 2**(k - 1)
            if (l /= 0 .and. l /= kf) return
            lv = int(l)
            if (e%range == PROG_PREP) then
                e%gs%instruct_control = ior(iand(e%gs%instruct_control, not(kf)), lv)
            else if (e%range == PROG_GLYPH .and. kf == 4) then
                e%bcm = ieor(iand(lv, 4), 4)
                call compute_funcs()
            end if
        end subroutine instctrl
    end subroutine run

    ! ---- stack, storage, cvt

    subroutine grow_fdefs_e(e, fn)
        type(exec_t), intent(inout) :: e
        integer, intent(in) :: fn
        type(fdef_t), allocatable :: t(:)
        if (fn < 0 .or. fn > 65535) then
            e%failed = .true.
            return
        end if
        allocate (t(0:fn))
        t(0:ubound(e%fdefs, 1)) = e%fdefs
        call move_alloc(t, e%fdefs)
    end subroutine grow_fdefs_e

    ! ---------------------------------------------------------------- arithmetic

    pure integer(ik) function mul_fix(a, b)
        integer(ik), intent(in) :: a, b
        integer(ik) :: c
        c = (abs(a) * abs(b) + 32768_ik) / 65536_ik
        if ((a < 0) .neqv. (b < 0)) c = -c
        mul_fix = c
    end function mul_fix

    pure integer(ik) function muldiv(a, b, c)
        integer(ik), intent(in) :: a, b, c
        integer(ik) :: d
        if (c == 0) then
            muldiv = huge(1_ik) / 4
            return
        end if
        d = (abs(a) * abs(b) + abs(c) / 2) / abs(c)
        if ((a < 0) .neqv. (b < 0) .neqv. (c < 0)) d = -d
        muldiv = d
    end function muldiv

    pure integer(ik) function muldiv_trunc(a, b, c)
        integer(ik), intent(in) :: a, b, c
        integer(ik) :: d
        d = (abs(a) * abs(b)) / abs(c)
        if ((a < 0) .neqv. (b < 0) .neqv. (c < 0)) d = -d
        muldiv_trunc = d
    end function muldiv_trunc

    pure integer(ik) function mul_fix14(a, b)
        integer(ik), intent(in) :: a, b
        integer(ik) :: c
        c = (abs(a) * abs(b) + 8192_ik) / 16384_ik
        if ((a < 0) .neqv. (b < 0)) c = -c
        mul_fix14 = c
    end function mul_fix14

    pure integer(ik) function dot14(ax, ay, bx, by)
        integer(ik), intent(in) :: ax, ay, bx, by
        integer(ik) :: m
        m = ax * bx + ay * by
        dot14 = shifta(m + 8192_ik + merge(-1_ik, 0_ik, m < 0), 14)
    end function dot14

    pure integer(ik) function pix_round(x)
        integer(ik), intent(in) :: x
        pix_round = iand(x + 32_ik, -64_ik)
    end function pix_round

    pure integer(ik) function div_fix(a, b)
        integer(ik), intent(in) :: a, b
        integer(ik) :: q
        q = (abs(a) * 65536_ik + abs(b) / 2) / abs(b)
        if ((a < 0) .neqv. (b < 0)) q = -q
        div_fix = q
    end function div_fix

    pure subroutine normalize(x, y, v)
        !! FreeType's Normalize: FT_Vector_NormLen's 16.16 unit vector along
        !! (x, y), divided by 4 to 2.14; v stays as it was for (0, 0).
        integer(ik), intent(in) :: x, y
        integer(ik), intent(inout) :: v(2)
        integer(ik), parameter :: M32 = 4294967295_ik
        integer(ik) :: ax, ay, l, b, z, u, w, xi, yi
        integer :: sh, sx, sy
        if (x == 0 .and. y == 0) return
        sx = merge(-1, 1, x < 0)
        sy = merge(-1, 1, y < 0)
        ax = iand(abs(x), M32)
        ay = iand(abs(y), M32)
        if (ax == 0) then
            v = [0_ik, sy * 65536_ik / 4]
            return
        else if (ay == 0) then
            v = [sx * 65536_ik / 4, 0_ik]
            return
        end if
        l = iand(merge(ax + shifta(ay, 1), ay + shifta(ax, 1), ax > ay), M32)
        sh = 31 - msb(l)
        sh = sh - (15 + merge(1, 0, l >= shifta(2863311530_ik, sh)))
        if (sh > 0) then
            ax = iand(ishft(ax, sh), M32)
            ay = iand(ishft(ay, sh), M32)
            l = iand(merge(ax + shifta(ay, 1), ay + shifta(ax, 1), ax > ay), M32)
        else
            ax = shifta(ax, -sh)
            ay = shifta(ay, -sh)
            l = shifta(l, -sh)
        end if
        b = 65536_ik - i32(l)
        xi = i32(ax)
        yi = i32(ay)
        do
            u = iand(xi + shifta(xi * b, 16), M32)
            w = iand(yi + shifta(yi * b, 16), M32)
            z = -i32(u * u + w * w) / 512
            z = i32(z * shifta(65536_ik + b, 8)) / 65536
            b = b + z
            if (z <= 0) exit
        end do
        v = [sx * u / 4, sy * w / 4]
    contains
        pure integer(ik) function i32(a)
            integer(ik), intent(in) :: a
            i32 = iand(a, M32)
            if (i32 >= 2147483648_ik) i32 = i32 - 4294967296_ik
        end function i32
        pure integer function msb(a)
            integer(ik), intent(in) :: a
            msb = 63 - leadz(a)
        end function msb
    end subroutine normalize

    pure integer(ik) function sext16(a)
        integer(ik), intent(in) :: a
        sext16 = iand(a, 65535_ik)
        if (sext16 >= 32768) sext16 = sext16 - 65536
    end function sext16

    pure integer function sbyte(b)
        integer, intent(in) :: b
        sbyte = b
        if (b >= 128) sbyte = b - 256
    end function sbyte


    integer function zone_arg(a)
        integer(ik), intent(in) :: a
        zone_arg = int(a)
        if (a /= 0 .and. a /= 1) zone_arg = 1
    end function zone_arg

end module fortfont_tt_hint
