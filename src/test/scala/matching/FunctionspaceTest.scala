package matching

import org.scalatest.FunSuite
import matching.TypeDef._
import matching.Functionspace.GaleShapleySolver

class FunctionspaceTest extends FunSuite {
  test( "Example 2 from Paper" ) {
    val prefM: Pref = Map[ Boy, List[ Girl ] ](
      1 -> List( 1, 2, 3, 4 ),
      2 -> List( 1, 4, 3, 2 ),
      3 -> List( 2, 1, 3, 4 ),
      4 -> List( 4, 2, 3, 1 )
    )
    val prefW: Pref = Map[ Girl, List[ Boy ] ](
      1 -> List( 4, 3, 1, 2 ),
      2 -> List( 2, 4, 1, 3 ),
      3 -> List( 4, 1, 2, 3 ),
      4 -> List( 3, 2, 1, 4 )
    )
    val alloc: Alloc = Map( )
    assert( GaleShapleySolver( prefM, prefW ).toList.sortBy( _._1 ).toMap == Map( 1 -> 3, 2 -> 4, 3 -> 1, 4 -> 2 ) )
  }
}
