Module numbers_module

  Implicit None

  Integer, Parameter :: wp = Selected_real_kind( 14, 70 )

End Module numbers_module

Module box_module

  Use numbers_module, Only : wp

  Implicit None

  Type, Public :: box
     Integer                                    :: n
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
    Allocate( A%r( 1:3, 1:n_inc ) )
    A%r = Huge( A%r )

  End Subroutine create

  Subroutine add_entry( A, r )
    
    Class( box ),                   Intent( InOut ) :: A
    Real( wp   ), Dimension( 1:3 ), Intent( In    ) :: r

    Real( wp ), Dimension( :, : ), Allocatable :: r_tmp

    If( A%n == Size( A%r, Dim = 2 ) ) Then
       r_tmp = A%r
       Deallocate( A%r )
       Allocate( A%r( 1:3, 1:A%n + n_inc ) )
       A%r = Huge( A%r )
       A%r( :, 1:A%n ) = r_tmp
    End If
    A%n = A%n + 1
    A%r( :, A%n ) = r

  End Subroutine add_entry

  Subroutine delete_entry( A, entry )

    Class( box ), Intent( InOut ) :: A
    Integer     , Intent( In    ) :: entry

    A%r( :, entry:Size( A%r, Dim = 2 ) - 1 ) = A%r( :, entry +1: )
    A%r( :, Size( A%r, Dim = 2 ) ) = Huge( A%r )
    A%n = A%n - 1

  End Subroutine delete_entry

End Module box_module

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

  Function direct_to_frac( A, r ) Result( f )

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

Program hsmc

  Use numbers_module, Only : wp
  Use lattice_module, Only : lattice
  Use box_module    , Only : box
  
  Implicit None

  Integer, Parameter :: n_refine = 10000
  Integer, Parameter :: n_bin = 100

  Type( lattice ) :: lat_vecs

  Type( box ), Dimension( :, :, : ), Allocatable :: boxes

  Real( wp ), Dimension( :, : ), Allocatable :: r_lat

  Real( wp ), Dimension( 1:3, 1:3 ) :: v

  Real( wp ), Dimension( 1:3 ) :: f
  Real( wp ), Dimension( 1:3 ) :: r, r_new
  Real( wp ), Dimension( 1:3 ) :: rand
  Real( wp ), Dimension( 1:3 ) :: swap
  Real( wp ), Dimension( 1:3 ) :: g_offset
  Real( wp ), Dimension( 1:3 ) :: rij

  Real( wp ), Dimension( 0:n_bin ) :: rdf

  Real( wp ) :: rho 
  Real( wp ) :: sigma
  Real( wp ) :: ax, ay, az
  Real( wp ) :: rand1
  Real( wp ) :: move_fac
  Real( wp ) :: rij_sq
  Real( wp ) :: accept_frac
  Real( wp ) :: r_min
  Real( wp ) :: dr, dr_inv, norm, sep

  Integer :: n, n_lat, nt, nover
  Integer :: nbx, nby, nbz
  Integer :: bx, by, bz
  Integer :: bx_new, by_new, bz_new
  Integer :: nx, ny, nz
  Integer :: iat
  Integer :: i, j
  Integer :: ix, iy, iz
  Integer :: inx, iny, inz
  Integer :: ibx, iby, ibz
  Integer :: igx, igy, igz
  Integer :: accept, accept_tot
  Integer :: imove
  Integer :: bin
  Integer :: start, finish, rate

  Integer( Selected_int_kind( 18 ) ) :: mc_step

  Logical :: overlap

  Write( *, * ) 'Vectors ?'
  Read ( *, * ) v

  Call lat_vecs%create( v )

  Write( *, * ) 'Volume = ', lat_vecs%V
  Write( *, * ) 'Direct lattice vectors'
  Write( *, '( 3( 3( f12.6, 1x ) / ) )' ) lat_vecs%dir_vecs
  Write( *, * ) 'Inverse lattice vectors'
  Write( *, '( 3( 3( f12.6, 1x ) / ) )' ) lat_vecs%inv_vecs

  Write( *, * ) 'Rho, n'
  Read ( *, * ) rho, n

  sigma = ( rho * lat_vecs%V / Real( n, wp ) ) ** ( 1.0_wp / 3.0_wp )
  Write( *, * ) 'Sigma = ', sigma

  ax = Sqrt( Dot_product( lat_vecs%dir_vecs( :, 1  ), lat_vecs%dir_vecs( :, 1  ) ) )
  ay = Sqrt( Dot_product( lat_vecs%dir_vecs( :, 2  ), lat_vecs%dir_vecs( :, 2  ) ) )
  az = Sqrt( Dot_product( lat_vecs%dir_vecs( :, 3  ), lat_vecs%dir_vecs( :, 3  ) ) )

  nbx = 1 + Int( ax / ( 3.0_wp * sigma ) )
  nby = 1 + Int( ay / ( 3.0_wp * sigma ) )
  nbz = 1 + Int( az / ( 3.0_wp * sigma ) )

  Write( *, * ) 'Nboxes = ', nbx, nby, nbz

  Allocate( boxes( 0:nbx - 1, 0:nby - 1, 0:nbz - 1 ) )

  Do i = 1, 50
     nx = i
     ny = Int( nx * ay / ax )
     nz = Int( nx * az / ax )
     If( nx * ny * nz >= n ) Exit
  End Do

  Do ix = 0, nbx - 1
     Do iy = 0, nby - 1
        Do iz = 0, nbz - 1
           Call boxes( ix, iy, iz )%create()
        End Do
     End Do
  End Do

  n_lat = nx * ny * nz
  Allocate( r_lat( 1:3, 1:n_lat ) )
  nt = 0
  Do ix = 0, nx - 1
     f( 1 ) = ix * ( 1.0_wp / nx )
     Do iy = 0, ny - 1
        f( 2 ) = iy * ( 1.0_wp / ny )
        Do iz = 0, nz - 1
           f( 3 ) = iz * ( 1.0_wp / nz )
           nt = nt + 1
           r_lat( :, nt ) = lat_vecs%frac_to_direct( f )
        End Do
     End Do
  End Do

  nover = n_lat - n
  nt = n_lat
  Do i = 1, nover
     Call Random_number( rand1 )
     iat = Int( rand1 * nt ) + 1
     swap = r_lat( :, iat )
     r_lat( :, iat ) = r_lat( :, nt )
     r_lat( :,  nt ) = swap
     nt = nt - 1
  End Do

  ! Check minimum separtion is OK
  Call system_clock( start, rate )
  r_min = Huge( r_min )
  !$omp parallel default( none ) shared( n, lat_vecs, r_lat ) &
  !$omp                          private( i, j, igx, igy, igz, rij, rij_sq, g_offset ) &
  !$omp                          reduction( min:r_min )
  !$omp do
  Do i = 1, n - 1
     Do j = i + 1, n
        rij = r_lat( :, i ) - r_lat( :, j )
        Do inx = -1, 1
           Do iny = -1, 1
              Do inz = -1, 1
                 g_offset = Matmul( lat_vecs%dir_vecs, Real( [ inx, iny, inz ], wp ) )
                 If( Dot_product( rij + g_offset, rij + g_offset ) < r_min ) Then
                    r_min = Dot_product( rij + g_offset, rij + g_offset )
                 End If
              End Do
           End Do
        End Do
     End Do
  End Do
  !$omp end do
  !$omp end parallel
  Call system_clock( finish, rate )
  Write( *, * ) 'Minimum separation, sigma, ratio'
  Write( *, * ) Sqrt( r_min ), sigma,  Sqrt( r_min ) / sigma
  Write( *, * ) 'Check time: ', Real( finish - start ) / rate
  If( r_min < sigma * sigma ) Then
     Stop "Overlaps in original config"
  End If

  Do i = 1, n
     f = lat_vecs%direct_to_frac( r_lat( :, i ) )
     bx = Int( f( 1 ) * nbx )
     by = Int( f( 2 ) * nby )
     bz = Int( f( 3 ) * nbz )
     Call boxes( bx, by, bz )%add_entry( r_lat( :, i ) )
  End Do

  move_fac = 0.5_wp
  accept = 0
  accept_tot = 0
  Call system_clock( start, rate )
  Do mc_step = 1, 2000000000

     Do
        Call Random_number( rand )
        bx = Int( rand( 1 ) * nbx )
        by = Int( rand( 2 ) * nby )
        bz = Int( rand( 3 ) * nbz )
        If( boxes( bx, by, bz )%n /= 0 ) Exit
     End Do

     Call Random_number( rand1 )
     imove = Int( boxes( bx, by, bz )%n * rand1 ) + 1

     Call Random_number( rand )
     r_new = boxes( bx, by, bz )%r( :, imove ) + ( rand - 0.5_wp ) * sigma * move_fac
     r_new = lat_vecs%translate_to_reference( r_new )

     f = lat_vecs%direct_to_frac( r_new )
     bx_new = Int( f( 1 ) * nbx )
     by_new = Int( f( 2 ) * nby )
     bz_new = Int( f( 3 ) * nbz )

     overlap = .False.
     Outer_neighbour: Do inx = -1, 1
        Do iny = -1, 1
           Do inz = -1, 1
              ibx = bx_new + inx
              iby = by_new + iny
              ibz = bz_new + inz
              g_offset = 0.0_wp
              If( bx_new + inx == nbx ) Then
                 ibx = 0
                 g_offset = g_offset + lat_vecs%dir_vecs( :, 1 )
              End If
              If( bx_new + inx == -1 ) Then
                 ibx = nbx - 1
                 g_offset = g_offset - lat_vecs%dir_vecs( :, 1 )
              End If
              If( by_new + iny == nby ) Then
                 iby = 0
                 g_offset = g_offset + lat_vecs%dir_vecs( :, 2 )
              End If
              If( by_new + iny == -1 ) Then
                 iby = nby - 1
                 g_offset = g_offset - lat_vecs%dir_vecs( :, 2 )
              End If
              If( bz_new + inz == nbz ) Then
                 ibz = 0
                 g_offset = g_offset + lat_vecs%dir_vecs( :, 3 )
              End If
              If( bz_new + inz == -1 ) Then
                 ibz = nbz - 1
                 g_offset = g_offset - lat_vecs%dir_vecs( :, 3 )
              End If
              Do i = 1, boxes( ibx, iby, ibz )%n
                 If( inx == 0 .And. iny == 0 .And. inz == 0 .And. imove == i ) Cycle
                 r = boxes( ibx, iby, ibz )%r( :, i )
                 rij_sq = Dot_product( r + g_offset - r_new, r + g_offset - r_new )
                 overlap = rij_sq < sigma * sigma
                 If( overlap ) Then
                    Exit Outer_neighbour
                  End If
               End Do
           End Do
        End Do
     End Do Outer_neighbour

     If( .Not. overlap ) Then
        Call boxes( bx, by, bz )%delete_entry( imove )
        f = lat_vecs%direct_to_frac( r_new )
        bx = Int( f( 1 ) * nbx )
        by = Int( f( 2 ) * nby )
        bz = Int( f( 3 ) * nbz )
        Call boxes( bx, by, bz )%add_entry( r_new )
        accept = accept + 1
     End If

     If( Mod( mc_step, Int( n_refine, Kind( mc_step ) ) ) == 0 ) Then
        accept_frac = Real( accept, wp ) / n_refine
        If( accept_frac < 0.2_wp ) Then
           move_fac = move_fac / 1.05_wp
        Else
           move_fac = Min( move_fac * 1.05_wp, 1.0_wp )
        End If
        If( Mod( mc_step, Int( 50 * n_refine, Kind( mc_step ) ) ) == 0 ) Then
           Write( *, * ) mc_step, accept_frac, move_fac
        End If
        accept_tot = accept_tot + accept
        accept = 0
     End If

  End Do
  Call system_clock( finish, rate )
  Write( *, * ) 'MC time: ', Real( finish - start ) / rate

  Write( *, * ) 'Number of moves made: ', accept_tot

  nt = 0
  Do ibx = 0, nbx - 1
     Do iby = 0, nby - 1
        Do ibz = 0, nbz - 1
           Do i = 1, boxes( ibx, iby, ibz )%n
              nt = nt + 1
              r_lat( :, nt ) = boxes( ibx, iby, ibz )%r( :, i )
           End Do
        End Do
     End Do
  End Do

  ! Check minimum separation is still OK
  r_min = Huge( r_min )
  Call system_clock( start, rate )
  !$omp parallel default( none ) shared( n, lat_vecs, r_lat ) &
  !$omp                          private( i, j, igx, igy, igz, rij, rij_sq, g_offset ) &
  !$omp                          reduction( min:r_min )
  !$omp do
  Do i = 1, n - 1
     Do j = i + 1, n
        rij = r_lat( :, i ) - r_lat( :, j )
        Do igx = -1, 1
           Do igy = -1, 1
              Do igz = -1, 1
                 g_offset = Matmul( lat_vecs%dir_vecs, Real( [ igx, igy, igz ], wp ) )
                 rij_sq = Dot_product( rij + g_offset, rij + g_offset )
                 If( rij_sq < r_min ) Then
                    r_min = rij_sq
                 End If
              End Do
           End Do
        End Do
     End Do
  End Do
  !$omp end do
  !$omp end parallel
  Call system_clock( finish, rate )
  Write( *, * ) 'Minimum separation, sigma, ratio'
  Write( *, * ) Sqrt( r_min ), sigma, Sqrt( r_min ) / sigma
  Write( *, * ) 'Check time: ', Real( finish - start ) / rate
  If( r_min < sigma * sigma ) Then
     Stop "Overlaps in final config"
  End If

  ! Calculate an rdf
  rdf = 0.0_wp
  dr = 0.05_wp * sigma
  dr_inv = 1.0_wp / dr
  Do i = 1, n - 1
     Do j = i + 1, n
        rij = r_lat( :, i ) - r_lat( :, j )
        Do igx = -1, 1
           Do igy = -1, 1
              Do igz = -1, 1
                 g_offset = Matmul( lat_vecs%dir_vecs, Real( [ igx, igy, igz ], wp ) )
                 rij_sq = Dot_product( rij + g_offset, rij + g_offset )
                 sep = Sqrt( rij_sq )
                 bin = Int( sep * dr_inv )
                 If( bin >= Lbound( rdf, Dim = 1 ) .And. bin <= Ubound( rdf, Dim = 1 ) ) Then
                    rdf( bin ) = rdf( bin ) + 2.0_wp
                 End If
              End Do
           End Do
        End Do
     End Do
  End Do
  Do i = Lbound( rdf, Dim = 1 ), Ubound( rdf, Dim = 1 )
     sep = i * dr
     norm = ( sep + dr ) ** 3 - sep ** 3
     ! Just be a little careful about integer overflow
     norm = norm * n
     norm = norm * n
     norm = 3.0_wp / ( 16.0_wp * Atan( 1.0_wp ) * norm )
     rdf( i ) = rdf( i ) * norm
     Write( 11, * ) sep / sigma, rdf( i )
  End Do

  Write( 10, '( "n = ", i0, 1x, "V = ", f0.3, 1x, "rho = ", f0.3 )' ) n, lat_vecs%V, rho
  Do i = 1, 3
     Write( 10, '( 3( g26.20, 1x ) )' ) lat_vecs%dir_vecs( i, : )
  End Do
  Do i = 1, 3
     Write( 10, '( 3( g26.20, 1x ) )' ) lat_vecs%inv_vecs( i, : )
  End Do
  Write( 10, * ) n
  Do i = 1, n
     Write( 10, '( 3( g26.20, 1x ) )' ) r_lat( :, i )
  End Do

End Program hsmc
