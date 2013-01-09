package io.das.huffman

import org.scalatest.FunSpec

class HuffmanUnitTests extends FunSpec {


  describe ("frequency counting"){
    it ("should correctly count character frequencies"){
      val freqs = Huffman.countFrequency("foaaobar".toList)
      assert(freqs('f') === 1)
      assert(freqs('o') === 2)
      assert(freqs('b') === 1)
      assert(freqs('a') === 3)
      assert(freqs('r') === 1)

    }

    it("shouldn't report any frequencies for an empty iterator"){
      assert(Huffman.countFrequency(Nil) === Map.empty[Char,Int])
    }

  }

  describe("processUntil"){
    it("should terminate immediately if test is true"){
      val data = List(1,2,3)

      val processed = Huffman.processUntil[Int](data)(d => true)(xs => ???)
      assert(processed === data)
    }

    it("should not terminate until the test is true"){
      val data = List(1,2,3)

      def fx = (xs:List[Int]) => {
        if (xs.length < 2)
          xs
        else{
          xs.head + xs.tail.head :: xs.tail.tail
        }
      }


      val processed = Huffman.processUntil[Int](data)(_.length == 1)(fx)
      assert(processed.length === 1)
      assert(processed(0) === 6)
    }

  }

  describe("makeCodeTreeFromFreqs"){
    it("should produce a codetree with a single leaf for a frequency map with one key"){
      val freq: Map[Char,Int] = Map('a' -> 5)

      val tree = Huffman.makeCodeTreeFromFreqs(freq)
      assert(tree.chars.contains('a'))
      assert(tree.weight === 5)
      assert(tree.chars.size === 1)
    }

    it("should produce a codetree with a single fork and two leaves for a frequency map with three keys"){
      val freq: Map[Char,Int] = Map('a' -> 5, 'b' -> 3, 'c' -> 10)

      val tree = Huffman.makeCodeTreeFromFreqs(freq)
      assert(tree.chars.contains('a'))
      assert(tree.chars.contains('b'))
      assert(tree.chars.contains('c'))
      assert(tree.weight === 18)
      assert(tree.chars.size === 3)
    }

  }

  describe("code tree processor"){
    it("should return the original list when size is < 2")
    {
      val input = List(CodeTree.Leaf('a',1))
      assert(Huffman.processCodeTrees(input) === input)
    }

    it("should replace two leaves with one fork from the input list"){
      val input = List(CodeTree.Leaf('a',1),CodeTree.Leaf('b',2))
      val processed = Huffman.processCodeTrees(input)
      assert(processed.length === 1)
      assert(processed.head.isInstanceOf[CodeTree.Fork])
    }

    it("should produce a fork which points to the replaced leaves"){
      val input = List(CodeTree.Leaf('a',1),CodeTree.Leaf('b',2))
      val processed = Huffman.processCodeTrees(input)
      val results  = processed.head match{
        case CodeTree.Fork(left,right) => (left === input.head, right === input.tail.head)
        case _ => throw new Exception("CodeTree.Fork expected")
      }

      assert(results._1)
      assert(results._2)
    }
  }
}
