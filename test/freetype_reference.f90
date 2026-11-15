module freetype_reference
    !! FreeType 2, opened at run time, as the reference of the tests. Fields are
    !! read at the offsets of FreeType's public structures on LP64 systems
    !! (FT_FaceRec, FT_SizeRec, FT_GlyphSlotRec, FT_Bitmap; FreeType 2.15).
    use iso_c_binding
    use iso_fortran_env, only: int8, int16, int32, int64
    implicit none
    private
    public :: freetype_open, ft_face_t, ft_face_open, ft_set_size, ft_load, ft_glyph_t, &
        ft_metrics, ft_units, ft_kerning

    integer(c_int32_t), parameter, public :: FT_LOAD_DEFAULT = 0, FT_LOAD_NO_HINTING = 2, &
        FT_LOAD_RENDER = 4, FT_LOAD_TARGET_LIGHT = 65536, FT_LOAD_COLOR = 1048576

    type :: ft_face_t
        type(c_ptr) :: face = c_null_ptr
        integer(int8), allocatable :: data(:)
    end type ft_face_t

    type :: ft_glyph_t
        !! A rendered glyph: coverage, or premultiplied BGRA for colour glyphs.
        integer :: width = 0, height = 0, left = 0, top = 0, advance26 = 0, mode = 0
        integer(int8), allocatable :: pixels(:, :, :) ! (channels, width, height)
    end type ft_glyph_t

    abstract interface
        integer(c_int) function fn_init(library) bind(C)
            import :: c_int, c_ptr
            type(c_ptr), intent(out) :: library
        end function
        integer(c_int) function fn_new_memory_face(library, base, size, index, face) bind(C)
            import :: c_int, c_ptr, c_long
            type(c_ptr), value :: library, base
            integer(c_long), value :: size, index
            type(c_ptr), intent(out) :: face
        end function
        integer(c_int) function fn_set_pixel_sizes(face, w, h) bind(C)
            import :: c_int, c_ptr
            type(c_ptr), value :: face
            integer(c_int), value :: w, h
        end function
        integer(c_int) function fn_select_size(face, index) bind(C)
            import :: c_int, c_ptr
            type(c_ptr), value :: face
            integer(c_int), value :: index
        end function
        integer(c_int) function fn_load_char(face, code, flags) bind(C)
            import :: c_int, c_ptr, c_long, c_int32_t
            type(c_ptr), value :: face
            integer(c_long), value :: code
            integer(c_int32_t), value :: flags
        end function
        integer(c_int) function fn_char_index(face, code) bind(C)
            import :: c_int, c_ptr, c_long
            type(c_ptr), value :: face
            integer(c_long), value :: code
        end function
        integer(c_int) function fn_get_kerning(face, left, right, mode, vec) bind(C)
            import :: c_int, c_ptr, c_long
            type(c_ptr), value :: face
            integer(c_int), value :: left, right, mode
            integer(c_long), intent(out) :: vec(2)
        end function
    end interface

    interface
        type(c_ptr) function dlopen(name, flags) bind(C)
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), value :: flags
        end function
        type(c_funptr) function dlsym(handle, name) bind(C)
            import :: c_ptr, c_funptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
        end function
    end interface

    type(c_ptr), save :: library = c_null_ptr
    procedure(fn_init), pointer :: FT_Init_FreeType => null()
    procedure(fn_new_memory_face), pointer :: FT_New_Memory_Face => null()
    procedure(fn_set_pixel_sizes), pointer :: FT_Set_Pixel_Sizes => null()
    procedure(fn_select_size), pointer :: FT_Select_Size => null()
    procedure(fn_load_char), pointer :: FT_Load_Char => null()
    procedure(fn_char_index), pointer :: FT_Get_Char_Index => null()
    procedure(fn_get_kerning), pointer :: FT_Get_Kerning => null()

contains

    logical function freetype_open()
        type(c_ptr) :: lib
        lib = dlopen("libfreetype.so.6" // c_null_char, 2_c_int)
        if (.not. c_associated(lib)) lib = dlopen("libfreetype.6.dylib" // c_null_char, 2_c_int)
        freetype_open = c_associated(lib)
        if (.not. freetype_open) return
        call c_f_procpointer(dlsym(lib, "FT_Init_FreeType" // c_null_char), FT_Init_FreeType)
        call c_f_procpointer(dlsym(lib, "FT_New_Memory_Face" // c_null_char), FT_New_Memory_Face)
        call c_f_procpointer(dlsym(lib, "FT_Set_Pixel_Sizes" // c_null_char), FT_Set_Pixel_Sizes)
        call c_f_procpointer(dlsym(lib, "FT_Select_Size" // c_null_char), FT_Select_Size)
        call c_f_procpointer(dlsym(lib, "FT_Load_Char" // c_null_char), FT_Load_Char)
        call c_f_procpointer(dlsym(lib, "FT_Get_Char_Index" // c_null_char), FT_Get_Char_Index)
        call c_f_procpointer(dlsym(lib, "FT_Get_Kerning" // c_null_char), FT_Get_Kerning)
        freetype_open = FT_Init_FreeType(library) == 0
    end function freetype_open

    logical function ft_face_open(f, data) result(ok)
        !! A face from font bytes; the bytes are kept, as FreeType reads them later.
        type(ft_face_t), intent(inout), target :: f
        integer(int8), intent(in) :: data(:)
        integer(c_int) :: err
        f%data = data
        err = FT_New_Memory_Face(library, c_loc(f%data), size(f%data, kind=c_long), 0_c_long, &
            f%face)
        ok = err == 0
        if (.not. ok) then
            f%face = c_null_ptr
            write (*, '(a, i0)') "FreeType cannot open the face, error ", err
        end if
    end function ft_face_open

    logical function ft_set_size(f, pixels) result(ok)
        !! Pixels per em, or the first strike of a bitmap-only face.
        type(ft_face_t), intent(in) :: f
        integer, intent(in) :: pixels
        ok = c_associated(f%face)
        if (.not. ok) return
        if (field_i32(f%face, 56) > 0 .and. .not. scalable(f)) then
            ok = FT_Select_Size(f%face, 0_c_int) == 0
        else
            ok = FT_Set_Pixel_Sizes(f%face, 0_c_int, int(pixels, c_int)) == 0
        end if
    end function ft_set_size

    logical function scalable(f)
        type(ft_face_t), intent(in) :: f
        ! FT_FaceRec.face_flags at offset 16, FT_FACE_FLAG_SCALABLE = 1.
        scalable = iand(field_i64(f%face, 16), 1_int64) /= 0
    end function scalable

    subroutine ft_units(f, upem, ascender, descender)
        type(ft_face_t), intent(in) :: f
        integer, intent(out) :: upem, ascender, descender
        upem = iand(field_i16(f%face, 136), 65535)
        ascender = field_i16(f%face, 138)
        descender = field_i16(f%face, 140)
    end subroutine ft_units

    subroutine ft_metrics(f, y_scale, ascender26, descender26, height26)
        !! FT_Size_Metrics of the current size: y_scale (16.16) and 26.6 values.
        type(ft_face_t), intent(in) :: f
        integer(int64), intent(out) :: y_scale, ascender26, descender26, height26
        type(c_ptr) :: size
        size = field_ptr(f%face, 160)
        y_scale = field_i64(size, 24 + 16)
        ascender26 = field_i64(size, 24 + 24)
        descender26 = field_i64(size, 24 + 32)
        height26 = field_i64(size, 24 + 40)
    end subroutine ft_metrics

    logical function ft_load(f, codepoint, flags, g) result(ok)
        type(ft_face_t), intent(in) :: f
        integer, intent(in) :: codepoint
        integer(c_int32_t), intent(in) :: flags
        type(ft_glyph_t), intent(out) :: g
        type(c_ptr) :: slot, buffer
        integer(int8), pointer :: rows(:)
        integer :: pitch, x, y, byte, ch
        ok = c_associated(f%face)
        if (.not. ok) return
        ok = FT_Load_Char(f%face, int(codepoint, c_long), flags) == 0
        if (.not. ok) return
        slot = field_ptr(f%face, 152)
        g%advance26 = int(field_i64(slot, 128))
        ! FT_GlyphSlotRec.format (offset 144) is 'bits' only for a rendered glyph.
        if (field_i32(slot, 144) /= int(z'62697473')) then
            allocate (g%pixels(1, 0, 0))
            return
        end if
        g%height = field_i32(slot, 152)
        g%width = field_i32(slot, 156)
        pitch = field_i32(slot, 160)
        buffer = field_ptr(slot, 168)
        g%mode = iand(int(field_i8(slot, 152 + 26)), 255)
        g%left = field_i32(slot, 192)
        g%top = field_i32(slot, 196)
        ch = merge(4, 1, g%mode == 7)
        allocate (g%pixels(ch, g%width, g%height))
        g%pixels = 0
        if (g%width == 0 .or. g%height == 0) return
        call c_f_pointer(buffer, rows, [abs(pitch) * g%height])
        do y = 1, g%height
            do x = 1, g%width
                select case (g%mode)
                case (1) ! FT_PIXEL_MODE_MONO: 1 bit per pixel
                    byte = iand(int(rows((y - 1) * pitch + (x - 1) / 8 + 1)), 255)
                    g%pixels(1, x, y) = merge(-1_int8, 0_int8, &
                        iand(ishft(byte, -(7 - mod(x - 1, 8))), 1) == 1)
                case (7) ! FT_PIXEL_MODE_BGRA
                    g%pixels(:, x, y) = rows((y - 1) * pitch + 4 * (x - 1) + 1: &
                        (y - 1) * pitch + 4 * x)
                case default
                    g%pixels(1, x, y) = rows((y - 1) * pitch + x)
                end select
            end do
        end do
    end function ft_load

    integer function ft_kerning(f, left, right) result(k26)
        !! Unfitted kerning (FT_KERNING_UNFITTED, the scaled value as HarfBuzz
        !! applies it) between two code points, 26.6.
        type(ft_face_t), intent(in) :: f
        integer, intent(in) :: left, right
        integer(c_long) :: vec(2)
        vec = 0
        if (FT_Get_Kerning(f%face, FT_Get_Char_Index(f%face, int(left, c_long)), &
            FT_Get_Char_Index(f%face, int(right, c_long)), 1_c_int, vec) /= 0) vec = 0
        k26 = int(vec(1))
    end function ft_kerning

    ! --- raw field access ------------------------------------------------------

    function field_ptr(base, offset) result(p)
        type(c_ptr), intent(in) :: base
        integer, intent(in) :: offset
        type(c_ptr) :: p
        type(c_ptr), pointer :: q
        call c_f_pointer(transfer(transfer(base, 0_c_intptr_t) + offset, base), q)
        p = q
    end function field_ptr

    integer(int64) function field_i64(base, offset)
        type(c_ptr), intent(in) :: base
        integer, intent(in) :: offset
        integer(int64), pointer :: q
        call c_f_pointer(transfer(transfer(base, 0_c_intptr_t) + offset, base), q)
        field_i64 = q
    end function field_i64

    integer function field_i32(base, offset)
        type(c_ptr), intent(in) :: base
        integer, intent(in) :: offset
        integer(int32), pointer :: q
        call c_f_pointer(transfer(transfer(base, 0_c_intptr_t) + offset, base), q)
        field_i32 = q
    end function field_i32

    integer function field_i16(base, offset)
        type(c_ptr), intent(in) :: base
        integer, intent(in) :: offset
        integer(int16), pointer :: q
        call c_f_pointer(transfer(transfer(base, 0_c_intptr_t) + offset, base), q)
        field_i16 = q
    end function field_i16

    integer(int8) function field_i8(base, offset)
        type(c_ptr), intent(in) :: base
        integer, intent(in) :: offset
        integer(int8), pointer :: q
        call c_f_pointer(transfer(transfer(base, 0_c_intptr_t) + offset, base), q)
        field_i8 = q
    end function field_i8

end module freetype_reference
