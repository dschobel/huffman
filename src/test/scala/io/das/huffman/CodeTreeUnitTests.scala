package io.das.huffman

import org.scalatest.FunSpec

class CodeTreeUnitTests extends FunSpec {

  describe("code tree equivalence"){
    it("should return true for code trees generated from identical sources"){
      val msg = "hello world" toList
      val t1 = CodeTree.makeCodeTreeFromText(msg)
      val t2 = CodeTree.makeCodeTreeFromText(msg)
      assert(t1 === t2)
    }
  }


  describe ("countFrequency"){
    it ("should correctly count character frequencies"){
      val freqs = CodeTree.countFrequency("foaaobar".toList)
      assert(freqs('f') === 1)
      assert(freqs('o') === 2)
      assert(freqs('b') === 1)
      assert(freqs('a') === 3)
      assert(freqs('r') === 1)
    }

    it("should still correctly count characters frequencies "){
      val freqs = CodeTree.countFrequency("this is an example of a huffman tree".toList)
      assert(freqs(' ') === 7)
      assert(freqs('a') === 4)
      assert(freqs('e') === 4)
      assert(freqs('f') === 3)
      assert(freqs('h') === 2)
      assert(freqs('i') === 2)
      assert(freqs('m') === 2)
      assert(freqs('n') === 2)
      assert(freqs('s') === 2)
      assert(freqs('t') === 2)
      assert(freqs('l') === 1)
      assert(freqs('o') === 1)
      assert(freqs('p') === 1)
      assert(freqs('r') === 1)
      assert(freqs('u') === 1)
      assert(freqs('x') === 1)

    }

    it("shouldn't report any frequencies for an empty iterator"){
      assert(CodeTree.countFrequency(Nil) === Map.empty[Char,Int])
    }

  }

  describe("processUntil"){
    it("should terminate immediately if test is true"){
      val data = List(1,2,3)

      val processed = CodeTree.processUntil[Int](data)(d => true)(xs => ???)
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


      val processed = CodeTree.processUntil[Int](data)(_.length == 1)(fx)
      assert(processed.length === 1)
      assert(processed(0) === 6)
    }

  }

  describe("makeCodeTreeFromFreqs"){
    it("should produce a codetree with a single leaf for a frequency map with one key"){
      val freq: Map[Char,Int] = Map('a' -> 5)

      val tree = CodeTree.makeCodeTreeFromFreqs(freq)
      assert(tree.chars.contains('a'))
      assert(tree.weight === 5)
      assert(tree.chars.size === 1)
    }

    it("should produce a correct tree with only two keys"){

      val freq: Map[Char,Int] = Map('c' -> 1,'b' -> 2)
      val tree = CodeTree.makeCodeTreeFromFreqs(freq)

      assert(tree.isInstanceOf[Fork])
      val  (lt,rt) = tree match { case Fork(left,right) => (left,right)}

      assert(lt.isInstanceOf[Leaf])
      assert(rt.isInstanceOf[Leaf])

      assert(lt.chars.contains('c'))
      assert(rt.chars.contains('b'))

    }

    it("it should build a code tree with highest weight key at the top level"){
      val freq: Map[Char,Int] = Map('a' -> 3,  'c' -> 2,'b' -> 1)

      val tree = CodeTree.makeCodeTreeFromFreqs(freq)
      assert(tree.chars.contains('a'))
      assert(tree.chars.contains('b'))
      assert(tree.chars.contains('c'))
      assert(tree.weight === 6)
      assert(tree.chars.size === 3)


      //leaf 'a' can either be to the left or the right of the root
      val  top = tree match {
        case Fork(Fork(lt,rt),leaf) =>leaf
        case Fork(leaf, Fork(lt,rt)) => leaf
        case _ => throw new Error("unexpected structure")
      }

      assert(top.chars === Set('a'))
    }
  }

  describe ("buildCodeTreeFromText"){

    //known good values from http://en.wikipedia.org/wiki/Huffman_coding
    it("should build the correct codetree"){
      val tree = CodeTree.makeCodeTreeFromText("this is an example of a huffman tree".toList)

      assert(tree.weight === 36)

      val (left,right) = tree match{ case Fork(left,right) => (left,right) }
      assert(left.weight === 16)
      assert(right.weight === 20)
    }

  }

  describe("combineCodeTrees"){
    it("should return the original list when size is < 2")
    {
      val input = List(Leaf('a',1))
      assert(CodeTree.combineCodeTrees(input) === input)
    }

    it("should replace two leaves with one fork from the input list"){
      val input = List(Leaf('a',1),Leaf('b',2))
      val processed = CodeTree.combineCodeTrees(input)
      assert(processed.length === 1)
      assert(processed.head.isInstanceOf[Fork])
    }

    it("should produce a fork which points to the replaced leaves"){
      val input = List(Leaf('a',1),Leaf('b',2))
      val processed = CodeTree.combineCodeTrees(input)
      val results  = processed.head match{
        case Fork(left,right) => (left === input.head, right === input.tail.head)
        case _ => throw new Exception("CodeTree.Fork expected")
      }

      assert(results._1)
      assert(results._2)
    }
  }

  describe("encoding and decoding"){
    it("should be possible to decode an encoded message"){
      val message = "this is my super secret message"
      val tree = CodeTree.makeCodeTreeFromText(message.toList)

      val encoded = tree.encode(message.toList)
      val decoded = tree.decode(encoded).mkString

      assert(decoded === message)
    }
  }
}
