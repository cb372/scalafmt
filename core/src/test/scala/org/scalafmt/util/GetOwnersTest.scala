package org.scalafmt.util

import org.scalafmt.util.TokenOps.TokenHash
import org.scalatest.FunSuite

class GetOwnersTest extends FunSuite {
  test("basic") {
    import scala.meta._
    val tree = "def identity[T] (t: T): T = t".parse[Stat].get
    val hash2tok: Map[TokenHash, Token] =
      tree.tokens.map(x => TokenOps.hash(x) -> x).toMap
    val slow = TreeOps.getOwners(tree)
    val fast = TreeOps.fastGetOwners(tree)
    val sslow = slow.map {
      case (k, v) => hash2tok(k).structure -> v
    }
    val ffast = fast.map {
      case (k, v) => hash2tok(k).structure -> v
    }
    logger.elem(sslow, ffast)
    tree.tokens.foreach { tok =>
      val k = TokenOps.hash(tok)
      val (l, r) = fast(k) -> slow(k)
      if (l != r) {
        logger.elem(tok, tok.structure, l, l.structure, r, r.structure)
        ???
      }
    }
  }

}
