! f(x) funksiyani [a,b] oraliqda integrallash
! bu usul Simpson usuli deb ataladi.
! Simpson usulida integralni sonli hisoblash 3-tartibli algebraik aniqlik darajasiga ega, ya'ni O(3)
! Simpson usuli funksiyani interpolyatsion ko`phadning ikkinchi hadigacha yoyilgan holatda,
! boshqacha aytganda parabolik interpolyatsiya orqali hisoblash imkonini beradi.
! Umumiy holda Simpson usulida integrallash formulasi quyidagicha:
!  _______________________________________________________
! |                                                       |
! |   F(x) = (b - a) / 6 (f(a) + 4 * f((a+b)/2) + f(b))   |
! |_______________________________________________________|

IMPLICIT NONE
DOUBLE PRECISION    :: a,c,h,integral
INTEGER             :: i,N

a=0.0D0
c=ACOS(-1.0D0)    ! Pi
N=5
h=(c-a)/(2*N)
integral=2*h/3.0D0*0.5D0*(f(a)+f(c))
DO  i=1,2*N-1,2
integral=integral+2*h/3.0D0*(2.0D0*f(a+i*h))
END DO
DO  i=2,2*N-2,2
integral=integral+2*h/3.0D0*f(a+i*h)
END DO
WRITE(*,*)"Natija:",integral
STOP

CONTAINS
FUNCTION f(x) RESULT(z)
IMPLICIT NONE
DOUBLE PRECISION, INTENT(IN)    :: x
DOUBLE PRECISION                :: z

z = SIN(3*x)+COS(2*x) !Integrallanuvchi funksiya. 
RETURN
END FUNCTION
END

