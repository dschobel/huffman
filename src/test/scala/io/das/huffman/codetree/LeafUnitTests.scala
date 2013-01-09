package io.das.huffman.codetree

import org.scalatest.{BeforeAndAfter, FunSpec}
import io.das.huffman.CodeTree.Leaf

/**
 * User: daniel
 * Date: 7/01/13
 * Time: 9:32 AM
 */
class LeafUnitTests extends FunSpec with BeforeAndAfter {

  var leaf: Leaf = _
  before{
    leaf = new Leaf('c',3)
  }
  describe("leaf"){
    it("should have a weight equal to value passed to constructor"){
      assert(leaf.weight === 3)
    }

    it("should reflect characters passed to constructor"){
      assert(leaf.chars === Set[Char]('c'))
    }

  }
}
