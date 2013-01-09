package io.das.huffman.codetree

import org.scalatest.{FunSpec, BeforeAndAfter}
import io.das.huffman.CodeTree.{Fork,Leaf}

/**
 * User: daniel
 * Date: 7/01/13
 * Time: 2:42 PM
 */
class ForkUnitTests extends FunSpec with BeforeAndAfter {

  var fork: Fork = _
  before {
    val left = new Leaf('a',3)
    val right = new Leaf('d',5)
    fork = new Fork(left,right, Set('a','d'))
  }

  describe("fork"){
    it("should have a weight equal to the sum of its leaves"){
      assert(fork.weight === 8)
    }

    it("should have a chars Set equal to the union of the two leaves' chars Sets"){
      assert(fork.chars === (fork.left.chars ++ fork.right.chars))
    }

    it("should reflect the summed weight of forks and leaves beneath the fork"){
      val other = new Leaf('z',1)
      val parent = new Fork(fork,other, other.chars ++ fork.chars)

      assert(parent.weight === 9)
      assert(parent.chars === Set('a','d','z'))
    }
  }

}
