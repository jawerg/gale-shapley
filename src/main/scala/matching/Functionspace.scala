package matching

import matching.TypeDef._

object Functionspace {

  @scala.annotation.tailrec
  def GaleShapleyIter( gss: GaleShapleyState ): Alloc = {
    if ( gss.is_stable ) gss.alloc
    else GaleShapleyIter( GaleShapleyState( gss.prefMp, gss.prefW, gss.allocp ) )
  }

  def GaleShapleySolver( prefM: PMap, prefW: PMap ): Alloc = {
    val alloc: Alloc = Map( )
    val GSS0 = GaleShapleyState( prefM, prefW, alloc )
    GaleShapleyIter( GSS0 )
  }
}