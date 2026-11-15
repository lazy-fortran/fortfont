program test_find
    !! Font discovery over a font folder written here: families case-insensitively,
    !! bitmap strikes of the asked size before the nearest, unknown families empty,
    !! the cache reused and refreshed when a file appears.
    use iso_c_binding, only: c_int, c_char, c_null_char
    use iso_fortran_env, only: int8
    use fortfont_find, only: font_find, font_index_rebuild
    use font_maker, only: outline_font, bitmap_font, colour_font
    implicit none
    interface
        integer(c_int) function setenv(name, value, overwrite) bind(C, name="setenv")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function
        integer(c_int) function mkdir(path, mode) bind(C, name="mkdir")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int), value :: mode
        end function
        integer(c_int) function rmdir(path) bind(C, name="rmdir")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function
    end interface
    character(len=:), allocatable :: root, path, matched
    integer :: failures, checked, face, k
    character(len=256) :: tmp
    character(len=:), allocatable :: files(:), dirs(:)

    failures = 0
    checked = 0
    call get_environment_variable("TMPDIR", tmp)
    if (len_trim(tmp) == 0) tmp = "/tmp"
    root = trim(tmp) // "/fortfont-test-find"
    dirs = [character(len=len(root) + 16) :: root, root // "/data", root // "/data/fonts", &
        root // "/data/fonts/sub", root // "/cache"]
    files = [character(len=len(root) + 32) :: root // "/data/fonts/outline.ttf", &
        root // "/data/fonts/sub/bitmap11.otb", root // "/data/fonts/sub/bitmap16.otb", &
        root // "/data/fonts/colour.ttf", root // "/cache/fortfont-index.txt"]
    call clean()
    do k = 1, size(dirs)
        if (mkdir(trim(dirs(k)) // c_null_char, int(o'755', c_int)) /= 0) continue
    end do
    if (setenv("FORTFONT_PATH" // c_null_char, root // "/data/fonts" // c_null_char, 1) /= 0 .or. &
        setenv("XDG_CACHE_HOME" // c_null_char, root // "/cache" // c_null_char, 1) /= 0) &
        error stop "setenv failed"
    call write_file(root // "/data/fonts/outline.ttf", outline_font(1000))
    call write_file(root // "/data/fonts/sub/bitmap11.otb", bitmap_font(11))
    call write_file(root // "/data/fonts/sub/bitmap16.otb", bitmap_font(16))
    call font_index_rebuild()

    call font_find("maker OUTLINE", "Regular", 0, path, face, matched)
    call check(ends_with(path, "/outline.ttf") .and. matched == "Maker Outline", &
        "family found case-insensitively")
    call font_find("Maker Bitmap", "Regular", 16, path, face)
    call check(ends_with(path, "/bitmap16.otb"), "strike of the asked size")
    call font_find("Maker Bitmap", "Regular", 11, path, face)
    call check(ends_with(path, "/bitmap11.otb"), "other strike of the asked size")
    call font_find("Maker Bitmap", "Regular", 13, path, face)
    call check(ends_with(path, "/bitmap11.otb"), "nearest strike otherwise")
    call font_find("No Such Family", "Regular", 0, path, face)
    call check(len(path) == 0, "unknown family gives no path")

    ! A new file appears: the cache is refreshed for it, the rest reused.
    call write_file(root // "/data/fonts/colour.ttf", colour_font(20))
    call font_index_rebuild()
    call font_find("Maker Colour", "", 0, path, face)
    call check(ends_with(path, "/colour.ttf"), "new font found")
    call font_find("Maker Outline", "", 0, path, face)
    call check(ends_with(path, "/outline.ttf"), "old fonts still found")

    call clean()
    write (*, '(a, i0, a, i0, a)') "find: ", checked, " checks, ", failures, " failures"
    if (failures > 0) error stop 1

contains

    subroutine clean()
        !! The files and folders of an earlier run, deepest first.
        integer :: k, u, ios
        do k = 1, size(files)
            open (newunit=u, file=trim(files(k)), status="old", iostat=ios)
            if (ios == 0) close (u, status="delete")
        end do
        do k = size(dirs), 1, -1
            if (rmdir(trim(dirs(k)) // c_null_char) /= 0) continue
        end do
    end subroutine clean

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        checked = checked + 1
        if (ok) return
        failures = failures + 1
        write (*, '(a)') "FAIL: " // what
    end subroutine check

    logical function ends_with(s, tail)
        character(*), intent(in) :: s, tail
        ends_with = .false.
        if (len(s) < len(tail)) return
        ends_with = s(len(s) - len(tail) + 1:) == tail
    end function ends_with

    subroutine write_file(path, bytes)
        character(*), intent(in) :: path
        integer(int8), intent(in) :: bytes(:)
        integer :: u
        open (newunit=u, file=path, access="stream", form="unformatted", status="replace")
        write (u) bytes
        close (u)
    end subroutine write_file

end program test_find
