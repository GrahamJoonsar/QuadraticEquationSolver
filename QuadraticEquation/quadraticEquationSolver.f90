! This is a program that processes quadratic equations 

subroutine getInput (a, b, c, lowerBound, upperBound, fileName)
    implicit none
    ! Declaring vars
    integer, parameter :: ikind = selected_real_kind(p=15)
    real(kind = ikind) :: a, b, c
    integer :: lowerBound, upperBound
    character(len = 20) :: fileName
    ! Getting the input
    print *, "Solving the quadratic equation : a(x)**2 + bx + c"
    print *, "Input A: "
    read *, a
    print *, "Input B: "
    read *, b
    print *, "Input C: "
    read *, c
    print *, "Lower bound on the x values (integer)"
    read *, lowerBound
    print *, "Upper bound on the x values (integer)"
    read *, upperBound
    print *, "File name: "
    read *, fileName
end subroutine getInput

subroutine solveEquation(a, b, c, x1, x2)
    implicit none
    ! Declaring vars
    integer, parameter :: ikind = selected_real_kind(p=15)
    real(kind = ikind) :: a, b, c, x1, x2
    ! THis is the actual equation
    x1 = ((-b) - sqrt(b ** 2 - (4 * a * c)))/(2 * a)
    x2 = ((-b) + sqrt(b ** 2 - (4 * a * c)))/(2 * a)
end subroutine solveEquation

subroutine logValuesToFile(a, b, c, lowerBound, upperBound, fileName, x1, x2)
    implicit none
    ! Declare variables
    integer, parameter :: ikind = selected_real_kind(p=15)
    real(kind = ikind) :: a, b, c, x1, x2
    integer :: lowerBound, upperBound, x
    character(len = 20) :: fileName
    ! Writing to the file
    open(10, file = fileName//".txt") ! Opening the txt file
    write(10,*) "a = ", a, "b = ", b, "c = ", c ! Creating the header
    write(10,*) "________________________________________________________________________________________"
    do x = lowerBound, upperBound
        if (x == x1 .or. x == x2) then
            write(10,*) "X: ", x, "    |     Corresponding Y: ", (a * (x ** 2)) + (b * x) + c, "  !Solution!"
            write(10,*) "---------------------------------------------------------------------------------"
        else
            write(10,*) "X: ", x, "    |     Corresponding Y: ", (a * (x ** 2)) + (b * x) + c
            write(10,*) "----------------------------------------------------------------------------------"
        end if
    end do
    close(10)
end subroutine logValuesToFile

program quadEquationSolver
    implicit none
    ! Adding precision to 15 decimals
    integer, parameter :: ikind = selected_real_kind(p=15)
    ! Declaring variables
    real(kind = ikind) :: a, b, c, x1, x2
    integer :: lowerBound, upperBound
    character(len = 20) :: fileName
    ! Getting user input
    call getInput(a, b, c, lowerBound, upperBound, fileName)
    ! Solving for the two possible solutions
    call solveEquation(a, b, c, x1, x2)
    ! Logging the values of the equation to a txt file
    call logValuesToFile(a, b, c, lowerBound, upperBound, fileName, x1, x2)
    ! Showing the solution to the quad
    print *, "Solution:"
    print *, "X1:", x1
    print *, "X2:", x2
    read *, ! Just sow the command prompt from closing instantly
end program quadEquationSolver