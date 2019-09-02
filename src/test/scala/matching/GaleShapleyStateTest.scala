package matching

import matching.TypeDef._
import org.scalatest.FunSuite

class GaleShapleyStateTest extends FunSuite {

  test( "Trivial one-to-one match" ) {
    val prefM: PMap = Map( 1 -> List( 1 ) )
    val prefW: PMap = Map( 1 -> List( 1 ) )
    assert(
      GaleShapleyState( prefM, prefW, Map( ) ).matches
        === Map( 1 -> 1 )
    )
  }

  test( "Empty matches" ) {
    val prefM = Map( 1 -> List( 2 ), 2 -> List( 1 ) )
    val prefW = Map( 1 -> List( 1 ), 2 -> List( 2 ) )
    val alloc: Alloc = Map( )
    assert(
      GaleShapleyState( prefM, prefW, alloc ).matches
        === Map( )
    )
  }
}
