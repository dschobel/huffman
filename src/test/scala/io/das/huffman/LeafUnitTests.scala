package io.das.huffman

import org.scalatest.{BeforeAndAfter, FunSpec}


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
