package matching

import matching.TypeDef._

case class Translator( boypref: Input, girlpref: Input ) {

  // generate indices and an according Encoder
  def translator( keys: Input, vals: Input ): Encoder = {
    ( keys.keySet ++ vals.values.flatten.toSet
      ).toList.sorted.zipWithIndex.toMap
  }

  // Define the relevant metrics for the Translator to work.
  val enc_boy: Encoder = translator( boypref, girlpref )
  val enc_girl: Encoder = translator( girlpref, boypref )
  val dec_boy: Decoder = enc_boy.toList.map( _.swap ).toMap
  val dec_girl: Decoder = enc_girl.toList.map( _.swap ).toMap

  def decode_alloc( alloc: Alloc ): Map[ String, String ] = {
    alloc.toList.map( x => (dec_girl( x._1 ), dec_boy( x._2 )) ).toMap
  }

  def encode_setup: (PMap, PMap) = {
    (
      boypref.toList.map( x => (enc_boy( x._1 ), x._2.map( enc_girl )) ).toMap,
      girlpref.toList.map( x => (enc_girl( x._1 ), x._2.map( enc_boy )) ).toMap
    )
  }

  def initialize_gamestate: GaleShapleyState = {
    val (prefM, prefW) = encode_setup
    GaleShapleyState( prefM, prefW, Map( ) )
  }
}
