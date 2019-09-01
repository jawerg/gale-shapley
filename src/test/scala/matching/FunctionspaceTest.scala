package matching

import matching.Functionspace._
import matching.TypeDef._
import org.scalatest.FunSuite

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
    assert(
      GaleShapleySolver( prefM, prefW ).toList.sortBy( _._1 ).toMap
        === Map( 1 -> 3, 2 -> 4, 3 -> 1, 4 -> 2 )
    )
  }

  test( "Symmetry in unique match problem" ) {
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
    assert(
      GaleShapleySolver( prefM, prefW ).toList.sortBy( _._1 ).toMap
        === GaleShapleySolver( prefW, prefM ).toList.sortBy( _._1 ).toMap
    )
  }

  // Late Reallocation assures that the program does end in "locally stable"
  // allocations, but traverses down the search tree until the match 4 -> 3 is
  // finally found.
  test( "Late Reallocation" ) {
    val prefM: Pref = Map(
      1 -> List( 1, 2, 3 ),
      2 -> List( 1, 2, 3 ),
      3 -> List( 1, 2, 3 ),
      4 -> List( 1, 2, 3 )
    )
    val prefW: Pref = Map(
      1 -> List( 1, 4 ),
      2 -> List( 2, 4 ),
      3 -> List( 4, 3 )
    )
    val alloc: Alloc = Map( 1 -> 1, 2 -> 2, 3 -> 3 )

    assert(
      GaleShapleyIter( GaleShapleyState( prefM, prefW, alloc ) )
        === Map( 1 -> 1, 2 -> 2, 3 -> 4 )
    )
  }
}
