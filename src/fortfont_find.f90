module fortfont_find
    !! Installed fonts without fontconfig: the font folders (or those named in
    !! FORTFONT_PATH, separated by ':') are scanned once,
    !! only the headers of each file are read (table directory, name, OS/2 and
    !! bitmap strike sizes), and the result is kept in a cache file that is
    !! refreshed for files whose size changed or that appeared.
    use iso_c_binding, only: c_ptr, c_char, c_null_char, c_associated, c_f_pointer, c_int, &
        c_null_ptr
    use iso_fortran_env, only: int8, int64
    implicit none
    private
    public :: font_entry_t, font_find, font_index, font_index_rebuild

    type :: font_entry_t
        character(len=:), allocatable :: path, family, style
        integer :: face = 0, weight = 400, size = 0
        logical :: italic = .false.
        integer, allocatable :: ppems(:) ! bitmap strike sizes; empty for outline fonts
        logical :: outlines = .true.
    end type font_entry_t

    type(font_entry_t), allocatable, save :: fonts(:)
    logical, save :: loaded = .false.
    character(*), parameter :: CACHE_VERSION = "fortfont-index 1"

    interface
        type(c_ptr) function opendir(name) bind(C, name="opendir")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: name(*)
        end function
        type(c_ptr) function readdir(dir) bind(C, name="readdir")
            import :: c_ptr
            type(c_ptr), value :: dir
        end function
        integer(c_int) function closedir(dir) bind(C, name="closedir")
            import :: c_ptr, c_int
            type(c_ptr), value :: dir
        end function
        integer(c_int) function c_mkdir(path, mode) bind(C, name="mkdir")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function
    end interface

#ifdef __APPLE__
    ! struct dirent of 64-bit-inode macOS: d_type at 20, d_name at 21.
    integer, parameter :: DIRENT_TYPE = 20, DIRENT_NAME = 21
#else
    ! struct dirent of glibc and musl on 64-bit Linux: d_type at 18, d_name at 19.
    integer, parameter :: DIRENT_TYPE = 18, DIRENT_NAME = 19
#endif
    integer, parameter :: DT_UNKNOWN = 0, DT_DIR = 4, DT_LNK = 10

contains

    subroutine font_find(family, style, pixel_size, path, face, matched)
        !! The installed font of `family` (case-insensitive) closest to `style`
        !! (weight and slant), with a bitmap strike of `pixel_size` preferred
        !! when given; `path` is empty when no font has this family.
        !! "sans-serif" and "monospace" name the system's default families.
        character(*), intent(in) :: family, style
        integer, intent(in) :: pixel_size
        character(len=:), allocatable, intent(out) :: path
        integer, intent(out) :: face
        character(len=:), allocatable, intent(out), optional :: matched
        character(*), parameter :: SANS(5) = [character(16) :: "Noto Sans", "DejaVu Sans", &
            "Liberation Sans", "Helvetica Neue", "Arial"]
        character(*), parameter :: MONO(6) = [character(16) :: "Noto Sans Mono", &
            "DejaVu Sans Mono", "Liberation Mono", "Cascadia Mono", "Menlo", "Consolas"]
        integer :: k
        path = ""
        face = 0
        if (present(matched)) matched = ""
        call font_index()
        select case (lower(trim(family)))
        case ("sans-serif", "sans")
            do k = 1, size(SANS)
                call best(trim(SANS(k)))
                if (len(path) > 0) return
            end do
        case ("monospace", "mono")
            do k = 1, size(MONO)
                call best(trim(MONO(k)))
                if (len(path) > 0) return
            end do
        case default
            call best(trim(family))
        end select
    contains
        subroutine best(name)
            character(*), intent(in) :: name
            integer :: k, score, top, want_weight
            logical :: want_italic
            call style_wanted(style, want_weight, want_italic)
            top = huge(1)
            do k = 1, size(fonts)
                if (lower(fonts(k)%family) /= lower(name)) cycle
                score = abs(fonts(k)%weight - want_weight)
                if (fonts(k)%italic .neqv. want_italic) score = score + 1000
                if (lower(fonts(k)%style) /= lower(trim(style))) score = score + 1
                if (pixel_size > 0 .and. size(fonts(k)%ppems) > 0) then
                    if (.not. any(fonts(k)%ppems == pixel_size)) score = score + 10000 + &
                        minval(abs(fonts(k)%ppems - pixel_size))
                end if
                if (score < top) then
                    top = score
                    path = fonts(k)%path
                    face = fonts(k)%face
                    if (present(matched)) matched = fonts(k)%family
                end if
            end do
        end subroutine best
    end subroutine font_find

    subroutine style_wanted(style, weight, italic)
        character(*), intent(in) :: style
        integer, intent(out) :: weight
        logical, intent(out) :: italic
        character(len=:), allocatable :: s
        s = lower(trim(style))
        italic = index_of(s, "italic") > 0 .or. index_of(s, "oblique") > 0
        weight = 400
        if (index_of(s, "thin") > 0) weight = 100
        if (index_of(s, "extralight") > 0 .or. index_of(s, "ultralight") > 0) weight = 200
        if (index_of(s, "light") > 0 .and. weight == 400) weight = 300
        if (index_of(s, "medium") > 0) weight = 500
        if (index_of(s, "semibold") > 0 .or. index_of(s, "demibold") > 0) weight = 600
        if (index_of(s, "bold") > 0 .and. weight == 400) weight = 700
        if (index_of(s, "extrabold") > 0 .or. index_of(s, "ultrabold") > 0) weight = 800
        if (index_of(s, "black") > 0 .or. index_of(s, "heavy") > 0) weight = 900
    end subroutine style_wanted

    pure integer function index_of(s, sub)
        character(*), intent(in) :: s, sub
        index_of = index(s, sub)
    end function index_of

    subroutine font_index()
        !! The index of installed fonts, from the cache when it is current.
        character(len=:), allocatable :: files(:)
        integer, allocatable :: sizes(:), slot(:), next(:)
        type(font_entry_t), allocatable :: cached(:), fresh(:)
        integer :: k, j, h, nslots, n
        logical :: hit
        if (loaded) return
        call list_font_files(files, sizes)
        call read_cache(cached)
        ! Cached entries chained by a hash of their path (several faces share one).
        nslots = 2 * size(cached) + 1
        allocate (slot(0:nslots - 1), next(size(cached)))
        slot = 0
        do j = 1, size(cached)
            h = modulo(hash(cached(j)%path), nslots)
            next(j) = slot(h)
            slot(h) = j
        end do
        allocate (fresh(4 * size(files) + 16))
        n = 0
        do k = 1, size(files)
            hit = .false.
            j = slot(modulo(hash(trim(files(k))), nslots))
            do while (j > 0)
                if (cached(j)%path == trim(files(k)) .and. cached(j)%size == sizes(k)) then
                    call push(cached(j))
                    hit = .true.
                end if
                j = next(j)
            end do
            if (.not. hit) call read_font_headers(trim(files(k)), sizes(k), fresh, n)
        end do
        if (allocated(fonts)) deallocate (fonts)
        allocate (fonts(n))
        fonts = fresh(1:n)
        call write_cache(fonts)
        loaded = .true.
    contains
        subroutine push(e)
            type(font_entry_t), intent(in) :: e
            type(font_entry_t), allocatable :: grown(:)
            if (n == size(fresh)) then
                allocate (grown(2 * n))
                grown(1:n) = fresh(1:n)
                call move_alloc(grown, fresh)
            end if
            n = n + 1
            fresh(n) = e
        end subroutine push
    end subroutine font_index

    subroutine font_index_rebuild()
        !! Forget the index and the cache, then scan again.
        character(len=:), allocatable :: cache
        integer :: u, ios
        cache = cache_path()
        open (newunit=u, file=cache, status="old", iostat=ios)
        if (ios == 0) close (u, status="delete")
        loaded = .false.
        call font_index()
    end subroutine font_index_rebuild

    ! --- folders ------------------------------------------------------------

    subroutine list_font_files(files, sizes)
        character(len=:), allocatable, intent(out) :: files(:)
        integer, allocatable, intent(out) :: sizes(:)
        character(len=1024), allocatable :: found(:)
        integer :: n, m, k
        character(len=:), allocatable :: home, data, paths
        allocate (found(256))
        n = 0
        ! FORTFONT_PATH (folders separated by ':') replaces the default folders.
        paths = env("FORTFONT_PATH")
        if (len(paths) > 0) then
            do while (len(paths) > 0)
                k = index(paths, ":")
                if (k == 0) k = len(paths) + 1
                if (k > 1) call walk(paths(:k - 1), 0)
                paths = paths(min(k + 1, len(paths) + 1):)
            end do
            call collect()
            return
        end if
        home = env("HOME")
        data = env("XDG_DATA_HOME")
        if (len(data) == 0 .and. len(home) > 0) data = home // "/.local/share"
        if (len(data) > 0) call walk(data // "/fonts", 0)
        if (len(home) > 0) call walk(home // "/.fonts", 0)
        call walk("/usr/share/fonts", 0)
        call walk("/usr/local/share/fonts", 0)
#ifdef __APPLE__
        if (len(home) > 0) call walk(home // "/Library/Fonts", 0)
        call walk("/Library/Fonts", 0)
        call walk("/System/Library/Fonts", 0)
#endif
        call collect()
    contains
        subroutine collect()
            m = 1
            do k = 1, n
                m = max(m, len_trim(found(k)))
            end do
            allocate (character(len=m) :: files(n))
            allocate (sizes(n))
            do k = 1, n
                files(k) = trim(found(k))
                inquire (file=trim(found(k)), size=sizes(k))
            end do
        end subroutine collect

        recursive subroutine walk(dir, depth)
            character(*), intent(in) :: dir
            integer, intent(in) :: depth
            type(c_ptr) :: d, e
            character(len=:), allocatable :: name, full
            integer :: kind
            if (depth > 8) return
            d = opendir(dir // c_null_char)
            if (.not. c_associated(d)) return
            do
                e = readdir(d)
                if (.not. c_associated(e)) exit
                call entry_of(e, name, kind)
                if (name == "." .or. name == "..") cycle
                full = dir // "/" // name
                ! Font files by name; anything else that may be a folder is
                ! opened as one (opendir fails harmlessly on files).
                if (is_font_name(name) .and. kind /= DT_DIR) then
                    call add(full)
                else if (kind == DT_DIR .or. kind == DT_UNKNOWN .or. kind == DT_LNK) then
                    call walk(full, depth + 1)
                end if
            end do
            if (closedir(d) /= 0) continue
        end subroutine walk

        subroutine add(path)
            character(*), intent(in) :: path
            character(len=1024), allocatable :: grown(:)
            if (n == size(found)) then
                allocate (grown(2 * n))
                grown(1:n) = found(1:n)
                call move_alloc(grown, found)
            end if
            n = n + 1
            found(n) = path
        end subroutine add
    end subroutine list_font_files

    subroutine entry_of(e, name, kind)
        !! d_name and d_type of a struct dirent.
        type(c_ptr), intent(in) :: e
        character(len=:), allocatable, intent(out) :: name
        integer, intent(out) :: kind
        character(kind=c_char), pointer :: bytes(:)
        integer :: k
        call c_f_pointer(e, bytes, [DIRENT_NAME + 256])
        kind = iachar(bytes(DIRENT_TYPE + 1))
        k = 0
        do while (k < 256)
            if (bytes(DIRENT_NAME + k + 1) == c_null_char) exit
            k = k + 1
        end do
        name = repeat(" ", k)
        do k = 1, len(name)
            name(k:k) = bytes(DIRENT_NAME + k)
        end do
    end subroutine entry_of

    logical function is_font_name(name)
        character(*), intent(in) :: name
        character(len=:), allocatable :: l
        integer :: n
        l = lower(name)
        n = len(l)
        is_font_name = .false.
        if (n < 5) return
        is_font_name = any(l(n - 3:n) == [".ttf", ".otf", ".otb", ".ttc", ".otc"])
    end function is_font_name

    ! --- headers -------------------------------------------------------------

    subroutine read_font_headers(path, file_size, out, n)
        !! Every face of a font file appended to out(1:n), reading only headers.
        character(*), intent(in) :: path
        integer, intent(in) :: file_size
        type(font_entry_t), allocatable, intent(inout) :: out(:)
        integer, intent(inout) :: n
        integer :: u, ios, nfaces, k, start
        integer(int8) :: head(16)
        open (newunit=u, file=path, access="stream", form="unformatted", status="old", &
            action="read", iostat=ios)
        if (ios /= 0) return
        read (u, pos=1, iostat=ios) head(1:12)
        if (ios /= 0) then
            close (u)
            return
        end if
        if (all(head(1:4) == int([116, 116, 99, 102], int8))) then
            nfaces = be32(head, 9)
            do k = 0, min(nfaces, 64) - 1
                start = read32(u, 13 + 4 * k)
                call read_face(u, start, k)
            end do
        else
            call read_face(u, 0, 0)
        end if
        close (u)
    contains
        subroutine read_face(u, start, face)
            integer, intent(in) :: u, start, face
            type(font_entry_t) :: e
            integer :: ntables, t, tag_off, offset, length, name_off, name_len, os2_off, os2_len
            integer :: loc_off, eblc_off, cblc_off, glyf_off
            integer(int8), allocatable :: dir(:), blob(:)
            character(4) :: tag
            integer :: ios2
            ntables = read16(u, start + 5)
            if (ntables <= 0 .or. ntables > 200) return
            allocate (dir(16 * ntables))
            read (u, pos=start + 13, iostat=ios2) dir
            if (ios2 /= 0) return
            name_off = 0
            name_len = 0
            os2_off = 0
            os2_len = 0
            eblc_off = 0
            cblc_off = 0
            glyf_off = 0
            do t = 0, ntables - 1
                tag_off = 16 * t
                tag = transfer(dir(tag_off + 1:tag_off + 4), tag)
                offset = be32(dir, tag_off + 9)
                length = be32(dir, tag_off + 13)
                select case (tag)
                case ("name")
                    name_off = offset
                    name_len = length
                case ("OS/2")
                    os2_off = offset
                    os2_len = length
                case ("EBLC")
                    eblc_off = offset
                case ("CBLC")
                    cblc_off = offset
                case ("glyf", "CFF ", "CFF2")
                    glyf_off = offset
                end select
            end do
            if (name_off == 0 .or. name_len <= 0 .or. name_off + name_len > file_size) return
            allocate (blob(name_len))
            read (u, pos=name_off + 1, iostat=ios2) blob
            if (ios2 /= 0) return
            e%family = name_record(blob, 16)
            if (len(e%family) == 0) e%family = name_record(blob, 1)
            e%style = name_record(blob, 17)
            if (len(e%style) == 0) e%style = name_record(blob, 2)
            if (len(e%family) == 0) return
            if (os2_off > 0 .and. os2_len >= 64) then
                e%weight = read16(u, os2_off + 5)
                e%italic = iand(read16(u, os2_off + 63), 1) /= 0
            else
                call style_wanted(e%style, e%weight, e%italic)
            end if
            allocate (e%ppems(0))
            loc_off = max(eblc_off, cblc_off)
            if (loc_off > 0) e%ppems = strike_sizes(u, loc_off)
            e%outlines = glyf_off > 0
            e%path = path
            e%face = face
            e%size = file_size
            if (n == size(out)) out = [out, out]
            n = n + 1
            out(n) = e
        end subroutine read_face
    end subroutine read_font_headers

    function strike_sizes(u, at) result(ppems)
        integer, intent(in) :: u, at
        integer, allocatable :: ppems(:)
        integer :: n, k
        integer(int8) :: b(1)
        n = read32(u, at + 5)
        allocate (ppems(max(0, min(n, 64))))
        do k = 1, size(ppems)
            read (u, pos=at + 8 + 48 * (k - 1) + 45 + 1) b
            ppems(k) = iand(int(b(1)), 255)
        end do
    end function strike_sizes

    function name_record(blob, id) result(s)
        !! A name of the name table (Windows English or Unicode, else Mac Roman).
        integer(int8), intent(in) :: blob(:)
        integer, intent(in) :: id
        character(len=:), allocatable :: s
        integer :: n, k, rec, platform, encoding, language, strings, best, rank, best_rank
        integer :: length, offset, j, c
        s = ""
        if (size(blob) < 6) return
        n = be16(blob, 3)
        strings = be16(blob, 5)
        best = -1
        best_rank = 99
        do k = 0, n - 1
            rec = 7 + 12 * k
            if (rec + 11 > size(blob)) exit
            if (be16(blob, rec + 6) /= id) cycle
            platform = be16(blob, rec)
            encoding = be16(blob, rec + 2)
            language = be16(blob, rec + 4)
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
        platform = be16(blob, best)
        length = be16(blob, best + 8)
        offset = strings + be16(blob, best + 10)
        if (offset + length > size(blob)) return
        if (platform == 1) then
            s = repeat(" ", length)
            do j = 1, length
                s(j:j) = achar(iand(int(blob(offset + j)), 255))
            end do
        else
            s = repeat(" ", length / 2)
            do j = 1, length / 2
                c = be16(blob, offset + 2 * j - 1)
                if (c < 128) then
                    s(j:j) = achar(c)
                else
                    s(j:j) = "?"
                end if
            end do
        end if
    end function name_record

    ! --- cache ----------------------------------------------------------------

    function cache_path() result(p)
        character(len=:), allocatable :: p, base
        base = env("XDG_CACHE_HOME")
        if (len(base) == 0) base = env("HOME") // "/.cache"
        p = base // "/fortfont-index.txt"
    end function cache_path

    subroutine read_cache(entries)
        type(font_entry_t), allocatable, intent(out) :: entries(:)
        character(len=4096) :: line
        integer :: u, ios, f(7), k, n
        type(font_entry_t) :: e
        type(font_entry_t), allocatable :: all(:)
        character(len=:), allocatable :: rest, field
        integer :: count
        allocate (entries(0), all(256))
        count = 0
        open (newunit=u, file=cache_path(), status="old", action="read", iostat=ios)
        if (ios /= 0) return
        read (u, '(a)', iostat=ios) line
        if (ios /= 0 .or. trim(line) /= CACHE_VERSION) then
            close (u)
            return
        end if
        do
            read (u, '(a)', iostat=ios) line
            if (ios /= 0) exit
            ! path TAB face TAB size TAB weight TAB italic TAB outlines TAB family TAB style TAB ppems
            rest = trim(line)
            e%path = next_field(rest)
            ios = 0
            do k = 1, 5
                field = next_field(rest)
                read (field, *, iostat=ios) f(k)
                if (ios /= 0) exit
            end do
            if (ios /= 0) cycle
            e%face = f(1)
            e%size = f(2)
            e%weight = f(3)
            e%italic = f(4) /= 0
            e%outlines = f(5) /= 0
            e%family = next_field(rest)
            e%style = next_field(rest)
            n = count_commas(rest)
            if (allocated(e%ppems)) deallocate (e%ppems)
            allocate (e%ppems(n))
            do k = 1, n
                field = next_field(rest, ",")
                read (field, *, iostat=ios) e%ppems(k)
            end do
            if (count == size(all)) all = [all, all]
            count = count + 1
            all(count) = e
        end do
        close (u)
        entries = all(1:count)
    end subroutine read_cache

    subroutine write_cache(entries)
        type(font_entry_t), intent(in) :: entries(:)
        integer :: u, ios, k, j
        character(len=:), allocatable :: path, line, dir
        character(len=16) :: num
        path = cache_path()
        dir = path(:index(path, "/", back=.true.) - 1)
        call make_dirs(dir)
        open (newunit=u, file=path, status="replace", action="write", iostat=ios)
        if (ios /= 0) return
        write (u, '(a)') CACHE_VERSION
        do k = 1, size(entries)
            associate (e => entries(k))
                line = e%path // achar(9) // itoa(e%face) // achar(9) // itoa(e%size) // &
                    achar(9) // itoa(e%weight) // achar(9) // itoa(merge(1, 0, e%italic)) // &
                    achar(9) // itoa(merge(1, 0, e%outlines)) // achar(9) // e%family // &
                    achar(9) // e%style // achar(9)
                do j = 1, size(e%ppems)
                    write (num, '(i0)') e%ppems(j)
                    line = line // trim(num) // ","
                end do
            end associate
            write (u, '(a)') line
        end do
        close (u)
    end subroutine write_cache

    subroutine make_dirs(dir)
        !! The folder and its missing parents (mode 0755).
        character(*), intent(in) :: dir
        integer :: k
        do k = 2, len(dir)
            if (dir(k:k) == "/") then
                if (c_mkdir(dir(:k - 1) // c_null_char, int(o'755', c_int)) /= 0) continue
            end if
        end do
        if (c_mkdir(dir // c_null_char, int(o'755', c_int)) /= 0) continue
    end subroutine make_dirs

    function next_field(rest, sep) result(field)
        character(len=:), allocatable, intent(inout) :: rest
        character, intent(in), optional :: sep
        character(len=:), allocatable :: field
        character :: s
        integer :: k
        s = achar(9)
        if (present(sep)) s = sep
        k = index(rest, s)
        if (k == 0) then
            field = rest
            rest = ""
        else
            field = rest(:k - 1)
            rest = rest(k + 1:)
        end if
    end function next_field

    pure integer function count_commas(s)
        character(*), intent(in) :: s
        integer :: k
        count_commas = 0
        do k = 1, len(s)
            if (s(k:k) == ",") count_commas = count_commas + 1
        end do
    end function count_commas

    ! --- helpers ----------------------------------------------------------------

    integer function read16(u, pos)
        integer, intent(in) :: u, pos
        integer(int8) :: b(2)
        integer :: ios
        read (u, pos=pos, iostat=ios) b
        read16 = 0
        if (ios == 0) read16 = be16(b, 1)
    end function read16

    integer function read32(u, pos)
        integer, intent(in) :: u, pos
        integer(int8) :: b(4)
        integer :: ios
        read (u, pos=pos, iostat=ios) b
        read32 = 0
        if (ios == 0) read32 = be32(b, 1)
    end function read32

    pure integer function hash(s)
        !! FNV-1a of a string, non-negative.
        character(*), intent(in) :: s
        integer(int64) :: h
        integer :: k
        h = 2166136261_int64
        do k = 1, len(s)
            h = iand(ieor(h, int(iachar(s(k:k)), int64)) * 16777619_int64, 2147483647_int64)
        end do
        hash = int(h)
    end function hash

    pure integer function be16(b, at)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: at
        be16 = 256 * iand(int(b(at)), 255) + iand(int(b(at + 1)), 255)
    end function be16

    pure integer function be32(b, at)
        integer(int8), intent(in) :: b(:)
        integer, intent(in) :: at
        be32 = ior(ishft(be16(b, at), 16), be16(b, at + 2))
    end function be32

    function env(name) result(v)
        character(*), intent(in) :: name
        character(len=:), allocatable :: v
        integer :: n, st
        call get_environment_variable(name, length=n, status=st)
        v = ""
        if (st /= 0 .or. n == 0) return
        v = repeat(" ", n)
        call get_environment_variable(name, v)
    end function env

    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=16) :: b
        write (b, '(i0)') i
        s = trim(b)
    end function itoa

    pure function lower(s) result(t)
        character(*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: k, c
        t = s
        do k = 1, len(s)
            c = iachar(s(k:k))
            if (c >= 65 .and. c <= 90) t(k:k) = achar(c + 32)
        end do
    end function lower

end module fortfont_find
