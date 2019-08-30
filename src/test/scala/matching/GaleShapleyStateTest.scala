import org.scalatest.FunSuite
import matching.GaleShapleyState

class GaleShapleyStateTest extends FunSuite {
  test( "GaleShapleyState.matches" ) {
    assert( GaleShapleyState( Map( 1 -> List( 1 ) ), Map( 1 -> List( 1 ) ), Map( ) ).matches === Map( 1 -> 1 ) )
  }
}
