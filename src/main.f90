!
! © 2024. Triad National Security, LLC. All rights reserved.
!
! This program was produced under U.S. Government contract 89233218CNA000001
! for Los Alamos National Laboratory (LANL), which is operated by
! Triad National Security, LLC for the U.S. Department of Energy/National
! Nuclear Security Administration. All rights in the program are reserved
! by Triad National Security, LLC, and the U.S. Department of Energy/National
! Nuclear Security Administration. The Government is granted for itself
! and others acting on its behalf a nonexclusive, paid-up, irrevocable
! worldwide license in this material to reproduce, prepare. derivative works,
! distribute copies to the public, perform publicly and display publicly,
! and to permit others to do so.
!
!
! Author:
!    Kai Gao <kaigao@lanl.gov>
!

module lib_string

    implicit none

    interface num2str
        module procedure :: int2str
        module procedure :: real2str
    end interface num2str

contains

    ! Convert logical to string
    function bool2str(bool) result(str)

        logical, intent(in) :: bool
        character(len=:), allocatable :: str

        character(len=64) :: boolstr

        if (bool) then
            boolstr = '.true.'
        else
            boolstr = '.false.'
        end if

        allocate (character(len=len_trim(boolstr)) :: str)
        str = trim(adjustl(boolstr))

    end function bool2str

    ! Convert integer(2) to string
    function int2str(num, format) result(str)

        integer, intent(in) :: num
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: str

        character(len=64) :: numformat, numstr

        if (present(format)) then
            numformat = format
        else
            numformat = '(i6)'
        end if

        write (numstr, numformat) num

        allocate (character(len=len_trim(numstr)) :: str)
        str = trim(adjustl(numstr))

    end function int2str

    ! Convert real to string
    function real2str(num, format) result(str)

        real, intent(in) :: num
        character(len=*), intent(in), optional :: format
        character(len=:), allocatable :: str

        character(len=64) :: numformat, numstr

        if (present(format)) then
            numformat = format
        else
            numformat = '(f)'
        end if

        write (numstr, numformat) num

        allocate (character(len=len_trim(numstr)) :: str)
        str = trim(adjustl(numstr))

    end function real2str

    ! Remove leading spaces and tabs from string
    function tidy(str) result(w)

        character(len=*), intent(in) :: str

        character(len=:), allocatable :: t, w
        integer :: i

        ! allocate memory
        allocate (character(len=len_trim(adjustl(str))) :: t)
        t = trim(adjustl(str))

        ! first remove the leading tabs, achar(9) is for tab
        i = index(t, achar(9))
        do while (i == 1)
            t = t(2:)
            i = index(t, achar(9))
        end do

        ! then extract the tidy string
        allocate (character(len=len_trim(adjustl(t))) :: w)
        w = trim(adjustl(t))

    end function tidy

    ! Convert string from lowercase to uppercase
    function to_upper(strIn) result(strOut)

        character(len=*), intent(in) :: strIn
        character(len=len(strIn)) :: strOut
        integer :: i, j

        do i = 1, len(strIn)
            j = iachar(strIn(i:i))
            if (j >= iachar("a") .and. j <= iachar("z")) then
                strOut(i:i) = achar(iachar(strIn(i:i)) - 32)
            else
                strOut(i:i) = strIn(i:i)
            end if
        end do

    end function to_upper

    ! Convert string from uppercase to lower case
    function to_lower(strIn) result(strOut)

        character(len=*), intent(in) :: strIn
        character(len=len(strIn)) :: strOut
        integer :: i, j

        do i = 1, len(strIn)
            j = iachar(strIn(i:i))
            if (j >= iachar("A") .and. j <= iachar("Z")) then
                strOut(i:i) = achar(iachar(strIn(i:i)) + 32)
            else
                strOut(i:i) = strIn(i:i)
            end if
        end do

    end function to_lower

    ! Extract a real number from string
    function extract_float(str) result(f)

        character(len=*), intent(in) :: str
        real :: f

        double precision :: var
        integer :: var_int
        integer :: pos_dot, pos_les, pos_ues
        character(len=256) :: var_char

        pos_dot = index(str, '.')
        pos_les = index(str, 'e')
        pos_ues = index(str, 'E')

        if (pos_dot == 0 .and. pos_les == 0 .and. pos_ues == 0) then
            ! If the input stream is an integer
            read (str, *) var_int
            var = real(var_int)
        else if (pos_dot == 0 .and. pos_les /= 0 .and. pos_ues == 0) then
            ! If the input stream is in the form of xxxxexxxx,
            ! then add a .0 to the string to convert it to xxxx.0exxxx
            var_char = str(1:pos_les - 1)//'.0'//str(pos_les:len_trim(str))
            read (var_char, *) var
        else if (pos_dot == 0 .and. pos_les == 0 .and. pos_ues /= 0) then
            ! If the input stream is in the form of xxxxExxxx,
            ! then add a .0 to the string to convert it to xxxx.0Exxxx
            var_char = str(1:pos_ues - 1)//'.0'//str(pos_ues:len_trim(str))
            read (var_char, *) var
        else
            read (str, *) var
        end if

        f = real(var)

    end function extract_float

    ! Extract an integer number from string
    function extract_int(str) result(i)

        character(len=*), intent(in) :: str
        integer :: i

        i = int(extract_float(str), kind=4)

    end function extract_int

    ! Extract a logical from string
    function extract_logical(str)

        character(len=*), intent(in) :: str
        logical :: extract_logical

        if (tidy(str) == 'yes' &
                .or. tidy(str) == 'YES' &
                .or. tidy(str) == 'y' &
                .or. tidy(str) == 'Y' &
                .or. tidy(str) == 'true' &
                .or. tidy(str) == 'TRUE' &
                .or. tidy(str) == '.true.' &
                .or. tidy(str) == '.TRUE.' &
                .or. tidy(str) == 't' &
                .or. tidy(str) == 'T' &
                .or. tidy(str) == '1') then
            extract_logical = .true.
        else
            extract_logical = .false.
        end if

    end function extract_logical

    ! Extract a string from string, which is to handle cases containing ', ", of mixed of them
    function extract_string(str)

        character(len=*), intent(in) :: str
        character(len=len(trim(adjustl(str)))) :: tmpstring
        character(len=len(trim(adjustl(str)))) :: extract_string

        tmpstring = tidy(str)
        if ((index(tmpstring, '"') == 1 &
                .and. index(tmpstring, '"', back=.true.) == len(trim(adjustl(str)))) &
                .or. &
                (index(tmpstring, "'") == 1 &
                .and. index(tmpstring, "'", back=.true.) == len(trim(adjustl(str))))) then
            extract_string = tidy(tmpstring(2:len_trim(tmpstring) - 1))
        else if (index(tmpstring, "'") == 0 .and. index(tmpstring, '"') == 0) then
            extract_string = tidy(tmpstring)
        else
            extract_string = tmpstring
        end if

    end function extract_string

    function date_time_compact() result(w)

        integer :: date_time(8)
        character(len=12) :: real_clock(3)
        character(len=22) :: w

        call date_and_time(real_clock(1), real_clock(2), real_clock(3), date_time)
        write (w, '(a,i4.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') &
            ' [', date_time(1), '-', date_time(2), '-', date_time(3), &
            ' ', date_time(5), ':', date_time(6), ':', date_time(7), ']'

    end function date_time_compact

end module lib_string

module lib_readpar

    use lib_string

    implicit none

contains

    ! Read integer value from parameter file
    subroutine readpar_int(filename, parname, par, defaultval, required)

        character(len=*), intent(in) :: filename, parname
        integer, intent(inout) :: par
        integer, intent(in) :: defaultval
        logical, intent(in), optional :: required

        character(len=1024) :: line
        integer :: npar, funit, eqindex
        integer :: ioerr

        npar = 0
        open (newunit=funit, file=tidy(filename), status='old', action='read')
        do
            read (funit, '(a)', iostat=ioerr) line
            if (ioerr /= 0) exit
            if (tidy(line) == 'exit') exit
            if (len(tidy(line)) > 0) then
                eqindex = index(line, '=')
                if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                    par = extract_int(line(eqindex + 1:))
                    npar = npar + 1
                end if
            end if
        end do
        close (funit)

        if (npar == 0) then
            ! if not exists then set to default
            if (present(required)) then
                if (required) then
                    ! if required then stop
                    stop ' Error: Parameter '//tidy(parname)//' not found '
                else
                    ! if not required then set to default
                    par = defaultval
                end if
            else
                ! if not required then set to default value
                par = defaultval
            end if
        end if

    end subroutine readpar_int

    ! Read float value from parameter file
    subroutine readpar_float(filename, parname, par, defaultval, required)

        character(len=*), intent(in) :: filename, parname
        real, intent(inout) :: par
        real, intent(in) :: defaultval
        logical, intent(in), optional :: required

        character(len=1024) :: line
        integer :: npar, funit, eqindex
        integer :: ioerr

        npar = 0
        open (newunit=funit, file=tidy(filename), status='old', action='read')
        do
            read (funit, '(a)', iostat=ioerr) line
            if (ioerr /= 0) exit
            if (tidy(line) == 'exit') exit
            if (len(tidy(line)) > 0) then
                eqindex = index(line, '=')
                if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                    par = extract_float(tidy(line(eqindex + 1:)))
                    npar = npar + 1
                end if
            end if
        end do
        close (funit)

        if (npar == 0) then
            ! if not exists then set to default
            if (present(required)) then
                if (required) then
                    ! if required then stop
                    stop ' Error: parameter '//tidy(parname)//' not found '
                else
                    ! if not required then set to default
                    par = defaultval
                end if
            else
                ! if not required then set to default value
                par = defaultval
            end if
        end if

    end subroutine readpar_float

    ! Read integer value from parameter file
    subroutine readpar_logical(filename, parname, par, defaultval, required)

        character(len=*), intent(in) :: filename, parname
        logical, intent(inout) :: par
        logical, intent(in) :: defaultval
        logical, intent(in), optional :: required

        character(len=1024) :: line
        character(len=32) :: tvar
        integer :: npar, funit, eqindex
        integer :: ioerr

        npar = 0
        open (newunit=funit, file=tidy(filename), status='old', action='read')
        do
            read (funit, '(a)', iostat=ioerr) line
            if (ioerr /= 0) exit
            if (tidy(line) == 'exit') exit
            if (len(tidy(line)) > 0) then
                eqindex = index(line, '=')
                if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                    tvar = tidy(line(eqindex + 1:))
                    par = extract_logical(tvar)
                    npar = npar + 1
                end if
            end if
        end do
        close (funit)

        if (npar == 0) then
            ! if not exists then set to default
            if (present(required)) then
                if (required) then
                    ! if required then stop
                    stop ' Error: parameter '//tidy(parname)//' not found '
                else
                    ! if not required then set to default
                    par = defaultval
                end if
            else
                ! if not required then set to default value
                par = defaultval
            end if
        end if

    end subroutine readpar_logical

    ! Read string value from parameter file
    subroutine readpar_string(filename, parname, par, defaultval, required)

        character(len=*), intent(in) :: filename, parname
        character(len=*), intent(inout) :: par
        character(len=*), intent(in) :: defaultval
        logical, intent(in), optional :: required

        character(len=1024) :: line
        integer :: npar, funit, eqindex
        integer :: ioerr

        npar = 0
        open (newunit=funit, file=tidy(filename), status='old', action='read')
        do
            read (funit, '(a)', iostat=ioerr) line
            if (ioerr /= 0) exit
            if (tidy(line) == 'exit') exit
            if (len(tidy(line)) > 0) then
                eqindex = index(line, '=')
                if (to_lower(tidy(line(1:eqindex - 1))) == to_lower(tidy(parname))) then
                    par = tidy(extract_string(line(eqindex + 1:)))
                    npar = npar + 1
                end if
            end if
        end do
        close (funit)

        if (npar == 0) then
            ! if not exists then set to default
            if (present(required)) then
                if (required) then
                    ! if required then stop
                    stop ' Error: parameter '//tidy(parname)//' not found '
                else
                    ! if not required then set to default
                    par = defaultval
                end if
            else
                ! if not required then set to default value
                par = defaultval
            end if
        end if

    end subroutine readpar_string

end module lib_readpar

module lib_array

    use lib_string

    implicit none

contains

    subroutine alloc_array(array, n, pad, source)

        real, allocatable, dimension(:, :), intent(inout) :: array
        integer, dimension(1:4), intent(in) :: n
        integer, intent(in), optional :: pad
        real, dimension(:, :), intent(in), optional :: source

        integer :: l
        real, allocatable, dimension(:, :) :: w

        if (present(source)) then
            allocate (w(size(source, 1), size(source, 2)), source=source)
        end if

        if (present(pad)) then
            l = pad
        else
            l = 0
        end if

        if (allocated(array)) then
            deallocate (array)
        end if

        allocate (array(n(1) - l:n(2) + l, n(3) - l:n(4) + l))

        if (present(source)) then
            array(:, :) = w(:, :)
        else
            array = 0.0
        end if

    end subroutine alloc_array

    function load(filename, n1, n2, transp) result(w)

        integer, intent(in) :: n1, n2
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transp
        real, allocatable, dimension(:, :) :: w

        integer :: funit
        integer :: target_size

        target_size = n1*n2*4

        ! if filename exists and file fize sufficient
        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='read', access='stream', status='old')
        if (present(transp)) then
            if (transp) then
                allocate (w(1:n2, 1:n1))
                read (funit) w
                w = transpose(w)
            end if
        else
            allocate (w(1:n1, 1:n2))
            read (funit) w
        end if
        close (funit)

    end function load

    subroutine pad_array(w, pad, method, const)

        ! arguments
        real, allocatable, dimension(:, :), intent(inout) :: w
        integer, dimension(1:4), intent(in) :: pad
        character(len=*), dimension(1:4), intent(in), optional :: method
        real, intent(in), optional :: const

        ! local variables
        integer :: i, j
        integer :: n1beg, n1end, n2beg, n2end
        real, allocatable, dimension(:, :) :: wp
        integer :: l1, u1, l2, u2
        character(len=16), dimension(1:4) :: pad_method
        real :: pad_const

        if (present(const)) then
            pad_const = const
        else
            pad_const = 0.0
        end if

        ! bounds
        n1beg = lbound(w, 1)
        n1end = ubound(w, 1)
        n2beg = lbound(w, 2)
        n2end = ubound(w, 2)

        l1 = pad(1)
        u1 = pad(2)
        l2 = pad(3)
        u2 = pad(4)

        ! new array
        allocate (wp(n1beg - l1:n1end + u1, n2beg - l2:n2end + u2))
        !$omp parallel do private(i, j) collapse(2)
        do j = n2beg, n2end
            do i = n1beg, n1end
                wp(i, j) = w(i, j)
            end do
        end do
        !$omp end parallel do

        ! padding method
        if (present(method)) then
            pad_method = method
        else
            pad_method = ['edge', 'edge', 'edge', 'edge']
        end if

        ! axis 1 lower boundary
        select case (pad_method(1))
            case ('edge')
                !$omp parallel do private(i, j) collapse(2)
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j) = wp(n1beg, j)
                    end do
                end do
                !$omp end parallel do
            case ('symm')
                !$omp parallel do private(i, j) collapse(2)
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j) = wp(n1beg + i - 1, j)
                    end do
                end do
                !$omp end parallel do
            case ('const')
                !$omp parallel do private(i, j) collapse(2)
                do j = n2beg - l2, n2end + u2
                    do i = 1, l1
                        wp(n1beg - i, j) = pad_const
                    end do
                end do
                !$omp end parallel do
        end select

        ! axis 1 upper boundary
        select case (pad_method(2))
            case ('edge')
                !$omp parallel do private(i, j) collapse(2)
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j) = wp(n1end, j)
                    end do
                end do
                !$omp end parallel do
            case ('symm')
                !$omp parallel do private(i, j) collapse(2)
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j) = wp(n1end - i + 1, j)
                    end do
                end do
                !$omp end parallel do
            case ('const')
                !$omp parallel do private(i, j) collapse(2)
                do j = n2beg - l2, n2end + u2
                    do i = 1, u1
                        wp(n1end + i, j) = pad_const
                    end do
                end do
                !$omp end parallel do
        end select

        ! axis 2 lower boundary
        select case (pad_method(3))
            case ('edge')
                !$omp parallel do private(i, j) collapse(2)
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j) = wp(i, n2beg)
                    end do
                end do
                !$omp end parallel do
            case ('symm')
                !$omp parallel do private(i, j) collapse(2)
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j) = wp(i, n2beg + j - 1)
                    end do
                end do
                !$omp end parallel do
            case ('const')
                !$omp parallel do private(i, j) collapse(2)
                do j = 1, l2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2beg - j) = pad_const
                    end do
                end do
                !$omp end parallel do
        end select

        ! axis 2 upper boundary
        select case (pad_method(4))
            case ('edge')
                !$omp parallel do private(i, j) collapse(2)
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j) = wp(i, n2end)
                    end do
                end do
                !$omp end parallel do
            case ('symm')
                !$omp parallel do private(i, j) collapse(2)
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j) = wp(i, n2end - j + 1)
                    end do
                end do
                !$omp end parallel do
            case ('const')
                !$omp parallel do private(i, j) collapse(2)
                do j = 1, u2
                    do i = n1beg - l1, n1end + u1
                        wp(i, n2end + j) = pad_const
                    end do
                end do
                !$omp end parallel do
        end select

        ! reallocate input array
        w = wp

    end subroutine pad_array

    subroutine output_array(w, filename, transp)

        real, dimension(:, :), intent(in) :: w
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transp

        integer :: funit

        open (newunit=funit, file=tidy(filename), &
            form='unformatted', action='write', access='stream', &
            status='replace')

        if (present(transp)) then
            if (transp) then
                write (funit) transpose(w)
            end if
        else
            write (funit) w
        end if

        close (funit)

    end subroutine output_array

end module lib_array

module vars

    implicit none

#ifdef fd16
    integer, parameter :: fdhalf = 8
    real, parameter :: coef81 = 0.1259312e+1
    real, parameter :: coef82 = -0.1280347e+0
    real, parameter :: coef83 = 0.3841945e-1
    real, parameter :: coef84 = -0.1473229e-1
    real, parameter :: coef85 = 0.5924913e-2
    real, parameter :: coef86 = -0.2248618e-2
    real, parameter :: coef87 = 0.7179226e-3
    real, parameter :: coef88 = -0.1400855e-3
#endif

#ifdef fd8
    integer, parameter :: fdhalf = 4
    real, parameter :: coef41 = 0.1217990e+1
    real, parameter :: coef42 = -0.9382142e-1
    real, parameter :: coef43 = 0.1507536e-1
    real, parameter :: coef44 = -0.1700324e-2
#endif

    integer, parameter :: pml = 20
    real, parameter :: const_pi = atan(1.0d0)*4.0d0

    ! field variables
    real, allocatable, dimension(:, :) :: vx, vz, stressxx, stresszz, stressxz
    real, allocatable, dimension(:, :) :: memory_pdxxx, memory_pdxxz
    real, allocatable, dimension(:, :) :: memory_pdzxz, memory_pdzzz
    real, allocatable, dimension(:, :) :: memory_pdxvx, memory_pdzvx
    real, allocatable, dimension(:, :) :: memory_pdxvz, memory_pdzvz

    real, allocatable, dimension(:, :) :: vxd, vzd, stressxxd, stresszzd, stressxzd
    real, allocatable, dimension(:, :) :: memory_pdxxxd, memory_pdxxzd
    real, allocatable, dimension(:, :) :: memory_pdzxzd, memory_pdzzzd
    real, allocatable, dimension(:, :) :: memory_pdxvxd, memory_pdzvxd
    real, allocatable, dimension(:, :) :: memory_pdxvzd, memory_pdzvzd

    real, allocatable, dimension(:, :) :: snapvx, snapvz, ep, es
    real, allocatable, dimension(:) :: sx, sz, sf, rx, rz
    integer :: ns, nr
    real, allocatable, dimension(:, :) :: datap, datas, datax, dataz
    integer :: nx, nz
    real :: dx, dz
    integer :: nt
    real :: dt
    real :: idx, idz
    real, allocatable, dimension(:, :) :: vp, vs, rho, lambda2mu, lambda, mu, mask
    real :: f0
    character(len=1024) :: file_parameter, file_src, file_rec, file_mask
    character(len=1024) :: file_vp, file_vs, file_rho, dir_working

end module vars

module cpml

    use lib_array
    use vars

    implicit none

    integer, parameter :: npower_k = 2
    integer, parameter :: npower_d = 2
    integer, parameter :: npower_a = 1
    real :: R0, kmax, alphamax

    real, allocatable, dimension(:) :: axi, azi, bxi, bzi, kxi, kzi
    real, allocatable, dimension(:) :: axh, azh, bxh, bzh, kxh, kzh

    real, allocatable, dimension(:, :) :: axii
    real, allocatable, dimension(:, :) :: azii
    real, allocatable, dimension(:, :) :: bxii
    real, allocatable, dimension(:, :) :: bzii
    real, allocatable, dimension(:, :) :: kxii
    real, allocatable, dimension(:, :) :: kzii

    real, allocatable, dimension(:, :) :: axhh
    real, allocatable, dimension(:, :) :: azhh
    real, allocatable, dimension(:, :) :: bxhh
    real, allocatable, dimension(:, :) :: bzhh
    real, allocatable, dimension(:, :) :: kxhh
    real, allocatable, dimension(:, :) :: kzhh

    real, allocatable, dimension(:, :) :: axhi
    real, allocatable, dimension(:, :) :: azhi
    real, allocatable, dimension(:, :) :: bxhi
    real, allocatable, dimension(:, :) :: bzhi
    real, allocatable, dimension(:, :) :: kxhi
    real, allocatable, dimension(:, :) :: kzhi

    real, allocatable, dimension(:, :) :: axih
    real, allocatable, dimension(:, :) :: azih
    real, allocatable, dimension(:, :) :: bxih
    real, allocatable, dimension(:, :) :: bzih
    real, allocatable, dimension(:, :) :: kxih
    real, allocatable, dimension(:, :) :: kzih

contains

    subroutine damp_coef_mpml(vpx, vpz, ax, az, bx, bz, kx, kz, xdist, zdist, dx, dz, ratiox, ratioz)

        ! arguments
        real, intent(in) :: vpx, vpz, dx, dz, ratiox, ratioz
        real, intent(inout) :: ax, az, bx, bz, kx, kz
        real, intent(in) :: xdist, zdist

        ! local variables
        real :: dampx, dampz
        real :: alphax, alphaz
        real :: nd
        real :: wx, wz

        ax = 0.0
        az = 0.0
        bx = 0.0
        bz = 0.0
        kx = 1.0
        kz = 1.0

        nd = xdist
        dampx = vpx*(npower_d + 1.0)*log(1.0/R0)/(2.0*pml*dx)*nd**npower_d
        nd = zdist
        dampz = vpz*(npower_d + 1.0)*log(1.0/R0)/(2.0*pml*dz)*nd**npower_d

        wx = dampx + ratioz*dampz
        wz = dampz + ratiox*dampx
        dampx = wx
        dampz = wz

        nd = xdist
        alphax = alphamax*(1.0 - nd**npower_a) + 0.1*alphamax
        kx = 1.0 + (kmax - 1.0)*nd**npower_k
        ax = -2.0*abs(dt)*dampx/kx/(2.0 + abs(dt)*(alphax + dampx/kx))
        bx = (2.0 - abs(dt)*(alphax + dampx/kx))/(2.0 + abs(dt)*(alphax + dampx/kx))

        nd = zdist
        alphaz = alphamax*(1.0 - nd**npower_a) + 0.1*alphamax
        kz = 1.0 + (kmax - 1.0)*nd**npower_k
        az = -2.0*abs(dt)*dampz/kz/(2.0 + abs(dt)*(alphaz + dampz/kz))
        bz = (2.0 - abs(dt)*(alphaz + dampz/kz))/(2.0 + abs(dt)*(alphaz + dampz/kz))

    end subroutine damp_coef_mpml

    !
    !> @brief Compute damping coefficients for isotropic media
    !
    subroutine compute_cpml_damping_coef

        integer :: i, j
        real :: ax, az, bx, bz, kx, kz
        real :: xdisti, xdisth, zdisti, zdisth
        real :: vpmax
        real :: ratiox, ratioz

        ! Calculate C-PML constant
        alphamax = f0*const_pi
        R0 = 1.0e-5
        kmax = 1.0

        vpmax = max(maxval(vp(1, :)), maxval(vp(nx, :)), maxval(vp(:, 1)), maxval(vp(:, nz)))

        ! allocate memory for coefficient arrays
        call alloc_array(axii, [1, nx, 1, nz], pad=pml)
        call alloc_array(axih, [1, nx, 1, nz], pad=pml)
        call alloc_array(axhi, [1, nx, 1, nz], pad=pml)
        call alloc_array(axhh, [1, nx, 1, nz], pad=pml)

        call alloc_array(bxii, [1, nx, 1, nz], pad=pml)
        call alloc_array(bxih, [1, nx, 1, nz], pad=pml)
        call alloc_array(bxhi, [1, nx, 1, nz], pad=pml)
        call alloc_array(bxhh, [1, nx, 1, nz], pad=pml)

        call alloc_array(kxii, [1, nx, 1, nz], pad=pml)
        call alloc_array(kxih, [1, nx, 1, nz], pad=pml)
        call alloc_array(kxhi, [1, nx, 1, nz], pad=pml)
        call alloc_array(kxhh, [1, nx, 1, nz], pad=pml)

        call alloc_array(azii, [1, nx, 1, nz], pad=pml)
        call alloc_array(azih, [1, nx, 1, nz], pad=pml)
        call alloc_array(azhi, [1, nx, 1, nz], pad=pml)
        call alloc_array(azhh, [1, nx, 1, nz], pad=pml)

        call alloc_array(bzii, [1, nx, 1, nz], pad=pml)
        call alloc_array(bzih, [1, nx, 1, nz], pad=pml)
        call alloc_array(bzhi, [1, nx, 1, nz], pad=pml)
        call alloc_array(bzhh, [1, nx, 1, nz], pad=pml)

        call alloc_array(kzii, [1, nx, 1, nz], pad=pml)
        call alloc_array(kzih, [1, nx, 1, nz], pad=pml)
        call alloc_array(kzhi, [1, nx, 1, nz], pad=pml)
        call alloc_array(kzhh, [1, nx, 1, nz], pad=pml)

        kxii = 1.0
        kxih = 1.0
        kxhi = 1.0
        kxhh = 1.0

        kzii = 1.0
        kzih = 1.0
        kzhi = 1.0
        kzhh = 1.0

        !$omp parallel do private(i, j, ax, bx, kx, az, bz, kz, &
            !$omp xdisti, xdisth, zdisti, zdisth, ratiox, ratioz)
        do j = -pml + 1, nz + pml
            do i = -pml + 1, nx + pml

                xdisti = 0.0d0
                xdisth = 0.0d0
                zdisti = 0.0d0
                zdisth = 0.0d0

                if (i <= 1) then
                    xdisti = abs((i - 1 + 0.0d0)/pml)
                    xdisth = abs((i - 1 - 0.5d0)/pml)
                else if (i >= nx) then
                    xdisti = abs((i - nx + 0.0d0)/pml)
                    xdisth = abs((i - nx - 0.5d0)/pml)
                end if

                if (j <= 1) then
                    zdisti = abs((j - 1 + 0.0d0)/pml)
                    zdisth = abs((j - 1 - 0.5d0)/pml)
                else if (j >= nz) then
                    zdisti = abs((j - nz + 0.0d0)/pml)
                    zdisth = abs((j - nz - 0.5d0)/pml)
                end if

                ! compute only for boundaries
                if (.not. (i > 1 .and. i < nx .and. j > 1 .and. j < nz)) then

                    ! integer-integer
                    ratiox = 0.0
                    ratioz = 0.0

                    ! integer-integer
                    call damp_coef_mpml(vpmax, vpmax, ax, az, bx, bz, kx, kz, xdisti, zdisti, dx, dz, ratiox, ratioz)
                    axii(i, j) = ax
                    bxii(i, j) = bx
                    kxii(i, j) = kx
                    azii(i, j) = az
                    bzii(i, j) = bz
                    kzii(i, j) = kz

                    ! integer-half
                    call damp_coef_mpml(vpmax, vpmax, ax, az, bx, bz, kx, kz, xdisti, zdisth, dx, dz, ratiox, ratioz)
                    axih(i, j) = ax
                    bxih(i, j) = bx
                    kxih(i, j) = kx
                    azih(i, j) = az
                    bzih(i, j) = bz
                    kzih(i, j) = kz

                    ! half-integer
                    call damp_coef_mpml(vpmax, vpmax, ax, az, bx, bz, kx, kz, xdisth, zdisti, dx, dz, ratiox, ratioz)
                    axhi(i, j) = ax
                    bxhi(i, j) = bx
                    kxhi(i, j) = kx
                    azhi(i, j) = az
                    bzhi(i, j) = bz
                    kzhi(i, j) = kz

                    ! half-half
                    call damp_coef_mpml(vpmax, vpmax, ax, az, bx, bz, kx, kz, xdisth, zdisth, dx, dz, ratiox, ratioz)
                    axhh(i, j) = ax
                    bxhh(i, j) = bx
                    kxhh(i, j) = kx
                    azhh(i, j) = az
                    bzhh(i, j) = bz
                    kzhh(i, j) = kz

                end if

            end do
        end do
        !$omp end parallel do

    end subroutine compute_cpml_damping_coef

end module cpml

module wavefield

    use vars
    use cpml

    implicit none

#ifdef fd16

#define pdxvx_8 (coef81*(vx(i + 1, j) - vx(i, j)) \
    +coef82*(vx(i + 2, j) - vx(i - 1, j)) \
    +coef83*(vx(i + 3, j) - vx(i - 2, j)) \
    +coef84*(vx(i + 4, j) - vx(i - 3, j)) \
    +coef85*(vx(i + 5, j) - vx(i - 4, j)) \
    +coef86*(vx(i + 6, j) - vx(i - 5, j)) \
    +coef87*(vx(i + 7, j) - vx(i - 6, j)) \
    +coef88*(vx(i + 8, j) - vx(i - 7, j)))

#define pdzvz_8 (coef81*(vz(i, j + 1) - vz(i, j)) \
    +coef82*(vz(i, j + 2) - vz(i, j - 1)) \
    +coef83*(vz(i, j + 3) - vz(i, j - 2)) \
    +coef84*(vz(i, j + 4) - vz(i, j - 3)) \
    +coef85*(vz(i, j + 5) - vz(i, j - 4)) \
    +coef86*(vz(i, j + 6) - vz(i, j - 5)) \
    +coef87*(vz(i, j + 7) - vz(i, j - 6)) \
    +coef88*(vz(i, j + 8) - vz(i, j - 7)))

#define pdzvx_8 (coef81*(vx(i + 1, j + 1) - vx(i + 1, j)) \
    +coef82*(vx(i + 1, j + 2) - vx(i + 1, j - 1)) \
    +coef83*(vx(i + 1, j + 3) - vx(i + 1, j - 2)) \
    +coef84*(vx(i + 1, j + 4) - vx(i + 1, j - 3)) \
    +coef85*(vx(i + 1, j + 5) - vx(i + 1, j - 4)) \
    +coef86*(vx(i + 1, j + 6) - vx(i + 1, j - 5)) \
    +coef87*(vx(i + 1, j + 7) - vx(i + 1, j - 6)) \
    +coef88*(vx(i + 1, j + 8) - vx(i + 1, j - 7)))

#define pdxvz_8 (coef81*(vz(i + 1, j + 1) - vz(i, j + 1)) \
    +coef82*(vz(i + 2, j + 1) - vz(i - 1, j + 1)) \
    +coef83*(vz(i + 3, j + 1) - vz(i - 2, j + 1)) \
    +coef84*(vz(i + 4, j + 1) - vz(i - 3, j + 1)) \
    +coef85*(vz(i + 5, j + 1) - vz(i - 4, j + 1)) \
    +coef86*(vz(i + 6, j + 1) - vz(i - 5, j + 1)) \
    +coef87*(vz(i + 7, j + 1) - vz(i - 6, j + 1)) \
    +coef88*(vz(i + 8, j + 1) - vz(i - 7, j + 1)))

#define pdxxx_8 (coef81*(stressxx(i + 1, j) - stressxx(i, j)) \
    +coef82*(stressxx(i + 2, j) - stressxx(i - 1, j)) \
    +coef83*(stressxx(i + 3, j) - stressxx(i - 2, j)) \
    +coef84*(stressxx(i + 4, j) - stressxx(i - 3, j)) \
    +coef85*(stressxx(i + 5, j) - stressxx(i - 4, j)) \
    +coef86*(stressxx(i + 6, j) - stressxx(i - 5, j)) \
    +coef87*(stressxx(i + 7, j) - stressxx(i - 6, j)) \
    +coef88*(stressxx(i + 8, j) - stressxx(i - 7, j)))

#define pdxxz_8 (coef81*(stressxz(i + 1, j + 1) - stressxz(i, j + 1)) \
    +coef82*(stressxz(i + 2, j + 1) - stressxz(i - 1, j + 1)) \
    +coef83*(stressxz(i + 3, j + 1) - stressxz(i - 2, j + 1)) \
    +coef84*(stressxz(i + 4, j + 1) - stressxz(i - 3, j + 1)) \
    +coef85*(stressxz(i + 5, j + 1) - stressxz(i - 4, j + 1)) \
    +coef86*(stressxz(i + 6, j + 1) - stressxz(i - 5, j + 1)) \
    +coef87*(stressxz(i + 7, j + 1) - stressxz(i - 6, j + 1)) \
    +coef88*(stressxz(i + 8, j + 1) - stressxz(i - 7, j + 1)))

#define pdzzz_8 (coef81*(stresszz(i, j + 1) - stresszz(i, j)) \
    +coef82*(stresszz(i, j + 2) - stresszz(i, j - 1)) \
    +coef83*(stresszz(i, j + 3) - stresszz(i, j - 2)) \
    +coef84*(stresszz(i, j + 4) - stresszz(i, j - 3)) \
    +coef85*(stresszz(i, j + 5) - stresszz(i, j - 4)) \
    +coef86*(stresszz(i, j + 6) - stresszz(i, j - 5)) \
    +coef87*(stresszz(i, j + 7) - stresszz(i, j - 6)) \
    +coef88*(stresszz(i, j + 8) - stresszz(i, j - 7)))

#define pdzxz_8 (coef81*(stressxz(i + 1, j + 1) - stressxz(i + 1, j)) \
    +coef82*(stressxz(i + 1, j + 2) - stressxz(i + 1, j - 1)) \
    +coef83*(stressxz(i + 1, j + 3) - stressxz(i + 1, j - 2)) \
    +coef84*(stressxz(i + 1, j + 4) - stressxz(i + 1, j - 3)) \
    +coef85*(stressxz(i + 1, j + 5) - stressxz(i + 1, j - 4)) \
    +coef86*(stressxz(i + 1, j + 6) - stressxz(i + 1, j - 5)) \
    +coef87*(stressxz(i + 1, j + 7) - stressxz(i + 1, j - 6)) \
    +coef88*(stressxz(i + 1, j + 8) - stressxz(i + 1, j - 7)))

#endif

#ifdef fd8

#define pdxvx_4 (coef41*(vx(i + 1, j) - vx(i, j)) \
    +coef42*(vx(i + 2, j) - vx(i - 1, j)) \
    +coef43*(vx(i + 3, j) - vx(i - 2, j)) \
    +coef44*(vx(i + 4, j) - vx(i - 3, j)))

#define pdzvz_4 (coef41*(vz(i, j + 1) - vz(i, j)) \
    +coef42*(vz(i, j + 2) - vz(i, j - 1)) \
    +coef43*(vz(i, j + 3) - vz(i, j - 2)) \
    +coef44*(vz(i, j + 4) - vz(i, j - 3)))

#define pdzvx_4 (coef41*(vx(i + 1, j + 1) - vx(i + 1, j)) \
    +coef42*(vx(i + 1, j + 2) - vx(i + 1, j - 1)) \
    +coef43*(vx(i + 1, j + 3) - vx(i + 1, j - 2)) \
    +coef44*(vx(i + 1, j + 4) - vx(i + 1, j - 3)))

#define pdxvz_4 (coef41*(vz(i + 1, j + 1) - vz(i, j + 1)) \
    +coef42*(vz(i + 2, j + 1) - vz(i - 1, j + 1)) \
    +coef43*(vz(i + 3, j + 1) - vz(i - 2, j + 1)) \
    +coef44*(vz(i + 4, j + 1) - vz(i - 3, j + 1)))

#define pdxxx_4 (coef41*(stressxx(i + 1, j) - stressxx(i, j)) \
    +coef42*(stressxx(i + 2, j) - stressxx(i - 1, j)) \
    +coef43*(stressxx(i + 3, j) - stressxx(i - 2, j)) \
    +coef44*(stressxx(i + 4, j) - stressxx(i - 3, j)))

#define pdxxz_4 (coef41*(stressxz(i + 1, j + 1) - stressxz(i, j + 1)) \
    +coef42*(stressxz(i + 2, j + 1) - stressxz(i - 1, j + 1)) \
    +coef43*(stressxz(i + 3, j + 1) - stressxz(i - 2, j + 1)) \
    +coef44*(stressxz(i + 4, j + 1) - stressxz(i - 3, j + 1)))

#define pdzzz_4 (coef41*(stresszz(i, j + 1) - stresszz(i, j)) \
    +coef42*(stresszz(i, j + 2) - stresszz(i, j - 1)) \
    +coef43*(stresszz(i, j + 3) - stresszz(i, j - 2)) \
    +coef44*(stresszz(i, j + 4) - stresszz(i, j - 3)))

#define pdzxz_4 (coef41*(stressxz(i + 1, j + 1) - stressxz(i + 1, j)) \
    +coef42*(stressxz(i + 1, j + 2) - stressxz(i + 1, j - 1)) \
    +coef43*(stressxz(i + 1, j + 3) - stressxz(i + 1, j - 2)) \
    +coef44*(stressxz(i + 1, j + 4) - stressxz(i + 1, j - 3)))

#endif

    ! Average rho on a half_x or half_z node
#define rho_eff_x (0.5*sum(rho(i:i + 1, j)))
#define rho_eff_z (0.5*sum(rho(i, j:j + 1)))

    ! Average mu on half-half node
#define mu_eff_xz (4.0/sum(1.0/mu(i:i + 1, j:j + 1)))

contains

    !
    !> @brief Update wavefield in 2D isotropic elastic medium
    !
    subroutine update_wavefield(dt, &
            stressxx, stresszz, stressxz, vx, vz, &
            memory_pdxvx, memory_pdzvx, memory_pdxvz, memory_pdzvz, &
            memory_pdxxx, memory_pdzxz, memory_pdxxz, memory_pdzzz)

        ! Arguments
        real, intent(in) :: dt
        real, allocatable, dimension(:, :), intent(inout) :: &
            stressxx, stresszz, stressxz, vx, vz, &
            memory_pdxvx, memory_pdzvx, memory_pdxvz, memory_pdzvz, &
            memory_pdxxx, memory_pdzxz, memory_pdxxz, memory_pdzzz

        ! Local variables
        integer :: i, j
        real :: pdxvx, pdzvx, pdxvz, pdzvz
        real :: pdxxx, pdzxz, pdxxz, pdzzz

        ! central region
        !$omp parallel do private(i, j, pdxvx, pdzvx, pdxvz, pdzvz) collapse(2)
        do j = -pml + 1, nz + pml
            do i = -pml + 1, nx + pml

#ifdef fd16
                pdxvx = idx*pdxvx_8
                pdzvz = idz*pdzvz_8
#endif

#ifdef fd8
                pdxvx = idx*pdxvx_4
                pdzvz = idz*pdzvz_4
#endif

                if (i <= 0 .or. i >= nx + 1 .or. j <= 0 .or. j >= nz + 1) then
                    memory_pdxvx(i, j) = axii(i, j)*pdxvx + bxii(i, j)*memory_pdxvx(i, j)
                    pdxvx = (pdxvx + memory_pdxvx(i, j))/kxii(i, j)
                    memory_pdzvz(i, j) = azii(i, j)*pdzvz + bzii(i, j)*memory_pdzvz(i, j)
                    pdzvz = (pdzvz + memory_pdzvz(i, j))/kzii(i, j)
                end if

                stressxx(i, j) = stressxx(i, j) + dt*( &
                    +lambda2mu(i, j)*pdxvx &
                    + lambda(i, j)*pdzvz)

                stresszz(i, j) = stresszz(i, j) + dt*( &
                    +lambda(i, j)*pdxvx &
                    + lambda2mu(i, j)*pdzvz)

            end do
        end do
        !$omp end parallel do

        !$omp parallel do private(i, j, pdxvx, pdzvx, pdxvz, pdzvz) collapse(2)
        do j = -pml, nz + pml - 1
            do i = -pml, nx + pml - 1

#ifdef fd16
                pdzvx = idz*pdzvx_8
                pdxvz = idx*pdxvz_8
#endif

#ifdef fd8
                pdzvx = idz*pdzvx_4
                pdxvz = idx*pdxvz_4
#endif

                if (i + 1 <= 1 .or. i + 1 >= nx + 1 .or. j + 1 <= 1 .or. j + 1 >= nz + 1) then
                    memory_pdxvz(i + 1, j + 1) = axhh(i + 1, j + 1)*pdxvz + bxhh(i + 1, j + 1)*memory_pdxvz(i + 1, j + 1)
                    pdxvz = (pdxvz + memory_pdxvz(i + 1, j + 1))/kxhh(i + 1, j + 1)
                    memory_pdzvx(i + 1, j + 1) = azhh(i + 1, j + 1)*pdzvx + bzhh(i + 1, j + 1)*memory_pdzvx(i + 1, j + 1)
                    pdzvx = (pdzvx + memory_pdzvx(i + 1, j + 1))/kzhh(i + 1, j + 1)
                end if

                stressxz(i + 1, j + 1) = stressxz(i + 1, j + 1) + dt*mu_eff_xz*(pdzvx + pdxvz)

            end do
        end do
        !$omp end parallel do

        !$omp parallel do private(i, j, pdxxx, pdzxz, pdxxz, pdzzz) collapse(2)
        do j = -pml + 1, nz + pml
            do i = -pml, nx + pml - 1

#ifdef fd16
                pdxxx = idx*pdxxx_8
                pdzxz = idz*pdzxz_8
#endif

#ifdef fd8
                pdxxx = idx*pdxxx_4
                pdzxz = idz*pdzxz_4
#endif

                if (i + 1 <= 1 .or. i + 1 >= nx + 1 .or. j <= 0 .or. j >= nz + 1) then
                    memory_pdxxx(i + 1, j) = axhi(i + 1, j)*pdxxx + bxhi(i + 1, j)*memory_pdxxx(i + 1, j)
                    pdxxx = (pdxxx + memory_pdxxx(i + 1, j))/kxhi(i + 1, j)
                    memory_pdzxz(i + 1, j) = azhi(i + 1, j)*pdzxz + bzhi(i + 1, j)*memory_pdzxz(i + 1, j)
                    pdzxz = (pdzxz + memory_pdzxz(i + 1, j))/kzhi(i + 1, j)
                end if

                vx(i + 1, j) = vx(i + 1, j) + dt/rho_eff_x*(pdxxx + pdzxz)

            end do
        end do
        !$omp end parallel do

        !$omp parallel do private(i, j, pdxxx, pdzxz, pdxxz, pdzzz) collapse(2)
        do j = -pml, nz + pml - 1
            do i = -pml + 1, nx + pml

#ifdef fd16
                pdxxz = idx*pdxxz_8
                pdzzz = idz*pdzzz_8
#endif

#ifdef fd8
                pdxxz = idx*pdxxz_4
                pdzzz = idz*pdzzz_4
#endif

                if (i <= 0 .or. i >= nx + 1 .or. j + 1 <= 1 .or. j + 1 >= nz + 1) then
                    memory_pdxxz(i, j + 1) = axih(i, j + 1)*pdxxz + bxih(i, j + 1)*memory_pdxxz(i, j + 1)
                    pdxxz = (pdxxz + memory_pdxxz(i, j + 1))/kxih(i, j + 1)
                    memory_pdzzz(i, j + 1) = azih(i, j + 1)*pdzzz + bzih(i, j + 1)*memory_pdzzz(i, j + 1)
                    pdzzz = (pdzzz + memory_pdzzz(i, j + 1))/kzih(i, j + 1)
                end if

                vz(i, j + 1) = vz(i, j + 1) + dt/rho_eff_z*(pdxxz + pdzzz)

            end do
        end do
        !$omp end parallel do

    end subroutine update_wavefield

end module wavefield

program sensitivity

    use lib_array
    use lib_string
    use lib_readpar
    use vars
    use wavefield
    use cpml

    implicit none

    integer :: l, ishot, ir, irx, irz, isx, isz
    real :: minv, maxv, dtstable, f0clean, temp
    integer :: i, j, t
    real :: vs_avg, rho_avg
    character(len=12) :: modeling_type
    character(len=12), allocatable, dimension(:) :: name
    real :: a1, a2, w
    logical :: sensi(1:3)

    call get_command_argument(1, file_parameter)
    call readpar_int(file_parameter, 'nx', nx, 1, required=.true.)
    call readpar_int(file_parameter, 'nz', nz, 1, required=.true.)
    call readpar_float(file_parameter, 'dx', dx, 1.0, required=.true.)
    call readpar_float(file_parameter, 'dz', dz, 1.0, required=.true.)
    call readpar_int(file_parameter, 'nt', nt, 2000, required=.true.)
    call readpar_float(file_parameter, 'dt', dt, 1.0e-3, required=.true.)

    idx = 1.0d0/dx
    idz = 1.0d0/dz
    call readpar_logical(file_parameter, 'sensi_vp', sensi(1), .true.)
    call readpar_logical(file_parameter, 'sensi_vs', sensi(2), .true.)
    call readpar_logical(file_parameter, 'sensi_rho', sensi(3), .false.)
    name = pack(['vp ', 'vs ', 'rho'], mask=sensi)

    call readpar_string(file_parameter, 'file_vp', file_vp, '', required=.true.)
    call readpar_string(file_parameter, 'file_vs', file_vs, '', required=.true.)
    call readpar_string(file_parameter, 'file_rho', file_rho, '', required=.true.)
    call readpar_string(file_parameter, 'file_mask', file_mask, '', required=.true.)
    call readpar_string(file_parameter, 'dir_working', dir_working, '', required=.true.)
    call execute_command_line('mkdir -p '//tidy(dir_working))

    call readpar_int(file_parameter, 'ns', ns, 1, required=.true.)
    call readpar_int(file_parameter, 'nr', nr, 1, required=.true.)
    call readpar_string(file_parameter, 'file_src', file_src, '', required=.true.)
    call readpar_string(file_parameter, 'file_rec', file_rec, '', required=.true.)

    call readpar_string(file_parameter, 'modeling_type', modeling_type, 'sensitivity', required=.true.)

    allocate (sx(1:ns))
    allocate (sz(1:ns))
    allocate (sf(1:ns))
    allocate (rx(1:nr))
    allocate (rz(1:nr))
    open (3, file=tidy(file_src), action='read', status='old')
    do i = 1, ns
        read (3, *) sx(i), sz(i), sf(i)
    end do
    close (3)
    open (3, file=tidy(file_rec), action='read', status='old')
    do i = 1, nr
        read (3, *) rx(i), rz(i)
    end do
    close (3)

    ! get models
    vp = load(file_vp, nx, nz, transp=.true.)
    vs = load(file_vs, nx, nz, transp=.true.)
    rho = load(file_rho, nx, nz, transp=.true.)
    call pad_array(vp, [pml + 1, pml + 1, pml + 1, pml + 1])
    call pad_array(vs, [pml + 1, pml + 1, pml + 1, pml + 1])
    call pad_array(rho, [pml + 1, pml + 1, pml + 1, pml + 1])
    call alloc_array(lambda, [1, nx, 1, nz], pad=pml + 1, source=rho*(vp**2 - 2.0*vs**2))
    call alloc_array(mu, [1, nx, 1, nz], pad=pml + 1, source=rho*vs**2)
    call alloc_array(lambda2mu, [1, nx, 1, nz], pad=pml + 1, source=rho*vp**2)
    mask = load(file_mask, nx, nz, transp=.true.)

    do ishot = 1, ns

        print *, ' Shot = ', ishot, 'of', ns

        f0 = sf(ishot)

        minv = minval(vs)
        maxv = maxval(vp)

#ifdef fd16
        temp = abs(coef81) + abs(coef82) + abs(coef83) + abs(coef84) &
            + abs(coef85) + abs(coef86) + abs(coef87) + abs(coef88)
#endif

#ifdef fd8
        temp = abs(coef41) + abs(coef42) + abs(coef43) + abs(coef44)
#endif

        dtstable = 1.0/(temp*maxv*sqrt(1.0/dx**2 + 1.0/dz**2))
        f0clean = 0.5*minv/max(dx, dz)/(2.0/0.6)
        if (dt >= dtstable) then
            stop ' dt should < '//num2str(dtstable)
        end if
        if (f0 >= f0clean) then
            print *, ' f0 should < '//num2str(f0clean)
        end if

        call compute_cpml_damping_coef

        ! Iterate over m
        do l = 1, size(name)

            ! allocate memory
            call alloc_array(vx, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(vz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(stressxx, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(stresszz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(stressxz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxvx, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzvx, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxvz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzvz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxxx, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzxz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxxz, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzzz, [1, nx, 1, nz], pad=pml + fdhalf)

            call alloc_array(vxd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(vzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(stressxxd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(stresszzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(stressxzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxvxd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzvxd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxvzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzvzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxxxd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzxzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdxxzd, [1, nx, 1, nz], pad=pml + fdhalf)
            call alloc_array(memory_pdzzzd, [1, nx, 1, nz], pad=pml + fdhalf)

            call alloc_array(snapvx, [1, nx, 1, nz], pad=pml)
            call alloc_array(snapvz, [1, nx, 1, nz], pad=pml)
            call alloc_array(ep, [1, nx, 1, nz])
            call alloc_array(es, [1, nx, 1, nz])
            call alloc_array(datap, [1, 1, 1, nr])
            call alloc_array(datas, [1, 1, 1, nr])
            call alloc_array(datax, [1, nt, 1, nr])
            call alloc_array(dataz, [1, nt, 1, nr])

            ! start modeling
            do t = 1, nt

                ! ==============================================================
                call update_wavefield(dt, &
                    stressxx, stresszz, stressxz, vx, vz, &
                    memory_pdxvx, memory_pdzvx, memory_pdxvz, memory_pdzvz, &
                    memory_pdxxx, memory_pdzxz, memory_pdxxz, memory_pdzzz)

                ! Source term
                isx = nint(sx(ishot)/dx) + 1
                isz = nint(sz(ishot)/dz) + 1
                w = (const_pi*f0*((t - 1)*dt - 1.0/f0))**2
                w = (1 - 2.0*w)*exp(-w)
                vz(isx, isz) = vz(isx, isz) + dt*w*1.0e6

                ! ==============================================================
                if (modeling_type == 'sensitivity' .or. modeling_type == 'both') then

                    call update_wavefield(dt, &
                        stressxxd, stresszzd, stressxzd, vxd, vzd, &
                        memory_pdxvxd, memory_pdzvxd, memory_pdxvzd, memory_pdzvzd, &
                        memory_pdxxxd, memory_pdzxzd, memory_pdxxzd, memory_pdzzzd)

                    ! Source term
                    !$omp parallel do private(i, j, a1, a2, vs_avg, rho_avg)
                    do j = 1, nz
                        do i = 1, nx
                            if (mask(i, j) == 1) then
                                select case (name(l))
                                    case ('vp')
                                        a1 = (vx(i + 1, j) - vx(i, j))/dx
                                        a2 = (vz(i, j + 1) - vz(i, j))/dz
                                        stressxxd(i, j) = stressxxd(i, j) + dt*(a1 + a2)*2*vp(i, j)*rho(i, j)
                                        stresszzd(i, j) = stresszzd(i, j) + dt*(a1 + a2)*2*vp(i, j)*rho(i, j)
                                    case ('vs')
                                        a1 = (vx(i + 1, j) - vx(i, j))/dx
                                        a2 = (vz(i, j + 1) - vz(i, j))/dz
                                        stressxxd(i, j) = stressxxd(i, j) - dt*a2*4*vs(i, j)*rho(i, j)
                                        stresszzd(i, j) = stresszzd(i, j) - dt*a1*4*vs(i, j)*rho(i, j)
                                        a1 = (vx(i, j + 1) - vx(i, j))/dz
                                        a2 = (vz(i + 1, j) - vz(i, j))/dx
                                        vs_avg = 4.0/sum(1.0/vs(i:i + 1, j:j + 1))
                                        rho_avg = 0.25*sum(rho(i:i + 1, j:j + 1))
                                        stressxzd(i + 1, j + 1) = stressxzd(i + 1, j + 1) + dt*(a1 + a2)*2*vs_avg*rho_avg
                                    case ('rho')
                                        a1 = (vx(i + 1, j) - vx(i, j))/dx
                                        a2 = (vz(i, j + 1) - vz(i, j))/dz
                                        stressxxd(i, j) = stressxxd(i, j) - dt*(a1*vp(i, j)**2 + a2*(vp(i, j)**2 - 2*vs(i, j)**2))
                                        stresszzd(i, j) = stresszzd(i, j) - dt*(a2*vp(i, j)**2 + a1*(vp(i, j)**2 - 2*vs(i, j)**2))
                                        a1 = (vx(i, j + 1) - vx(i, j))/dz
                                        a2 = (vz(i + 1, j) - vz(i, j))/dx
                                        vs_avg = 4.0/sum(1.0/vs(i:i + 1, j:j + 1))
                                        stressxzd(i + 1, j + 1) = stressxzd(i + 1, j + 1) + dt*(a1 + a2)*vs_avg**2
                                end select
                            end if
                        end do
                    end do
                    !$omp end parallel do

                    ! ==============================================================
                    ! Compute sensitivity energy

                    !$omp parallel do private(i, j)
                    do j = -pml + 1, nz + pml
                        do i = -pml + 1, nx + pml
                            snapvx(i, j) = 0.5*(vxd(i + 1, j) + vxd(i, j))
                            snapvz(i, j) = 0.5*(vzd(i, j + 1) + vzd(i, j))
                        end do
                    end do
                    !$omp end parallel do

                    !$omp parallel do private(i, j)
                    do j = 1, nz
                        do i = 1, nx
                            ep(i, j) = ep(i, j) + ((snapvx(i + 1, j) - snapvx(i - 1, j))/dx + (snapvz(i, j + 1) - snapvz(i, j - 1))/dz)**2
                            es(i, j) = es(i, j) + ((snapvx(i, j + 1) - snapvx(i, j - 1))/dz - (snapvz(i + 1, j) - snapvz(i - 1, j))/dx)**2
                        end do
                    end do
                    !$omp end parallel do

                end if

                ! ==============================================================
                ! If requires output of regular wavefield, then save to datax and z

                if (modeling_type == 'wavefield' .or. modeling_type == 'both' .and. l == 1) then

                    !$omp parallel do private(ir, irx, irz)
                    do ir = 1, nr
                        irx = nint(rx(ir)/dx) + 1
                        irz = nint(rz(ir)/dz) + 1
                        datax(t, ir) = 0.5*(vx(irx + 1, irz) + vx(irx, irz))
                        dataz(t, ir) = 0.5*(vz(irx, irz + 1) + vz(irx, irz))
                    end do
                    !$omp end parallel do

                end if

                if (mod(t, nint(nt/10.0)) == 0) then
                    print *, date_time_compact(), ' Time step = ', t, 'of', nt, 'for ', tidy(name(l))
                end if

            end do

            if (modeling_type == 'sensitivity' .or. modeling_type == 'both') then

                ep = ep*rho(1:nx, 1:nz)*vp(1:nx, 1:nz)**2
                es = es*rho(1:nx, 1:nz)*vs(1:nx, 1:nz)**2
                call output_array(ep, tidy(dir_working)//'/all_sensitivity_p_wrt_' &
                    //tidy(name(l))//'_src_'//num2str(ishot)//'.bin', transp=.true.)
                call output_array(es, tidy(dir_working)//'/all_sensitivity_s_wrt_' &
                    //tidy(name(l))//'_src_'//num2str(ishot)//'.bin', transp=.true.)

                !$omp parallel do private(ir, irx, irz)
                do ir = 1, nr
                    irx = nint(rx(ir)/dx) + 1
                    irz = nint(rz(ir)/dz) + 1
                    datap(1, ir) = ep(irx, irz)
                    datas(1, ir) = es(irx, irz)
                end do
                !$omp end parallel do

                call output_array(datap, tidy(dir_working)//'/receiver_sensitivity_p_wrt_' &
                    //tidy(name(l))//'_src_'//num2str(ishot)//'.bin')
                call output_array(datas, tidy(dir_working)//'/receiver_sensitivity_s_wrt_' &
                    //tidy(name(l))//'_src_'//num2str(ishot)//'.bin')

            end if

            if (modeling_type == 'wavefield' .or. modeling_type == 'both' .and. l == 1) then

                call output_array(datax, tidy(dir_working)//'/wavefield_vx_src_'//num2str(ishot)//'.bin')
                call output_array(dataz, tidy(dir_working)//'/wavefield_vz_src_'//num2str(ishot)//'.bin')

            end if

            ! If modeling_type is wavefield only, then stop
            if (modeling_type == 'wavefield') then
                exit
            end if

        end do

    end do

end program sensitivity
