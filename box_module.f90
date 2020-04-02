Module box_module

  Use numbers_module, Only : wp

  Implicit None

  Type, Public :: box
     Integer                                    :: n
     Integer   , Dimension( :    ), Allocatable :: id
     Real( wp ), Dimension( :, : ), Allocatable :: r 
   Contains
     Procedure :: create
     Procedure :: add_entry
     Procedure :: delete_entry
  End type box

  Private

  Integer, Parameter :: n_inc = 30

Contains 

  Subroutine create( A )

    Class( box ), Intent( Out ) :: A

    A%n = 0
    Allocate( A%r ( 1:3, 1:n_inc ) )
    Allocate( A%id(      1:n_inc ) )
    A%r  = Huge( A%r  )
    A%id = Huge( A%id )
    
  End Subroutine create

  Subroutine add_entry( A, r, id )
    
    Class( box ),                   Intent( InOut ) :: A
    Real( wp   ), Dimension( 1:3 ), Intent( In    ) :: r
    Integer     ,                   Intent( In    ) :: id

    Real( wp ), Dimension( :, : ), Allocatable :: r_tmp

    Integer   , Dimension( : ), Allocatable :: id_tmp

    If( A%n == Size( A%r, Dim = 2 ) ) Then
       r_tmp  = A%r
       id_tmp = A%id
       Deallocate( A%r )
       Allocate( A%r( 1:3, 1:A%n + n_inc ) )
       A%r( :,       1:A%n ) = r_tmp
       A%r( :, A%n + 1:    ) = Huge( A%r )
       Deallocate( A%id )
       Allocate( A%id( 1:A%n + n_inc ) )
       A%id(       1:A%n ) = id_tmp
       A%id( A%n + 1:    ) = Huge( A%id )
    End If
    A%n = A%n + 1
    A%r ( :, A%n ) = r
    A%id(    A%n ) = id

  End Subroutine add_entry

  Subroutine delete_entry( A, entry )

    Class( box ), Intent( InOut ) :: A
    Integer     , Intent( In    ) :: entry

    A%r( :, entry:Size( A%r, Dim = 2 ) - 1 ) = A%r( :, entry +1: )
    A%r( :, Size( A%r, Dim = 2 ) ) = Huge( A%r )

    A%id( entry:Size( A%id ) - 1 ) = A%id( entry +1: )
    A%id( Size( A%id ) ) = Huge( A%id )

    A%n = A%n - 1

  End Subroutine delete_entry

End Module box_module

