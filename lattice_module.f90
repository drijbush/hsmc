Module lattice_module

  Use numbers_module, Only : wp

  Implicit None

  Type, Public :: lattice
     Real( wp )                        :: V
     Real( wp ), Dimension( 1:3, 1:3 ) :: dir_vecs
     Real( wp ), Dimension( 1:3, 1:3 ) :: inv_vecs
   Contains
     Procedure :: create
     Procedure :: direct_to_frac
     Procedure :: frac_to_direct
     Procedure :: translate_to_reference
  End type lattice

  Private

Contains

  Subroutine create( A, v )

    Class( lattice )                       , Intent(   Out  ) :: A
    Real ( wp      ), Dimension( 1:3, 1:3 ), Intent( In     ) :: v

    A%dir_vecs = v

    A%inv_vecs( :, 1 ) = cross( v( :, 2 ), v( :, 3 ) )
    A%inv_vecs( :, 2 ) = cross( v( :, 3 ), v( :, 1 ) )
    A%inv_vecs( :, 3 ) = cross( v( :, 1 ), v( :, 2 ) )

    A%V = Dot_product( v( :, 1 ), cross( v( :, 2 ), v( :, 3 ) ) )
    A%inv_vecs = A%inv_vecs / A%V
    A%V = Abs( A%V )

  Contains

    Pure Function cross( a, b ) Result( c )

      Real( wp ), Dimension( 1:3 ) :: c

      Real( wp ), Dimension( 1:3 ), Intent( In ) :: a
      Real( wp ), Dimension( 1:3 ), Intent( In ) :: b

      c( 1 ) =     a( 2 ) * b( 3 ) - a( 3 ) * b( 2 )
      c( 2 ) = - ( a( 1 ) * b( 3 ) - a( 3 ) * b( 1 ) )
      c( 3 ) =     a( 1 ) * b( 2 ) - a( 2 ) * b( 1 )

    End Function cross

  End Subroutine create

  Pure Function direct_to_frac( A, r ) Result( f )

    Real( wp ), Dimension( 1:3 ) :: f

    Class( lattice )                  , Intent( In ) :: A
    Real ( wp      ), Dimension( 1:3 ), Intent( In ) :: r

    f = Matmul( Transpose( A%inv_vecs ), r )

  End Function direct_to_frac

  Function frac_to_direct( A, f ) Result( r )

    Real( wp ), Dimension( 1:3 ) :: r

    Class( lattice )                  , Intent( In ) :: A
    Real ( wp      ), Dimension( 1:3 ), Intent( In ) :: f

    r = Matmul( A%dir_vecs, f )

  End Function frac_to_direct

  Function translate_to_reference( A, r ) Result( ref )

    Real( wp ), Dimension( 1:3 ) :: ref

    Class( lattice )                  , Intent( In ) :: A
    Real ( wp      ), Dimension( 1:3 ), Intent( In ) :: r

    Real( wp ), Dimension( 1:3 ) :: f

    f   = A%direct_to_frac( r )
    f   = f - Floor( f )
    ref = A%frac_to_direct( f )

  End Function translate_to_reference

End Module lattice_module

