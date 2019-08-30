import matching.TypeDef._
import matching.GaleShapleyState

// Example.
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

@scala.annotation.tailrec
def GaleShapleyIter( gss: GaleShapleyState ): Alloc = {
  if ( gss.is_stable ) gss.alloc
  else GaleShapleyIter( GaleShapleyState( gss.prefMp, gss.prefW, gss.alloc ++ gss.matches ) )
}

def GaleShapleySolver( prefM: Pref, prefW: Pref ): Alloc = {
  val alloc: Alloc = Map( )
  val GSS0 = GaleShapleyState( prefM, prefW, alloc )
  GaleShapleyIter( GSS0 )
}

GaleShapleySolver( prefM, prefW )