package matching

import org.scalatest.FunSuite
import matching.TypeDef._

class GaleShapleyStateTest extends FunSuite {
  test( "Trivial one-to-one match" ) {
    assert( GaleShapleyState( Map( 1 -> List( 1 ) ), Map( 1 -> List( 1 ) ), Map( ) ).matches === Map( 1 -> 1 ) )
  }

  test( "Empty matches" ) {
    val prefM = Map( 1 -> List( 2 ), 2 -> List( 1 ) )
    val prefW = Map( 1 -> List( 1 ), 2 -> List( 2 ) )
    val alloc: Alloc = Map( )
    assert( GaleShapleyState( prefM, prefW, alloc ).matches === Map( ) )
  }
}
