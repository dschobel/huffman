package io.das.huffman

import CodeTree._
import oracle.jrockit.jfr.events.Bits


object Huffman{

  //repeatedly apply 'fx' until condition 'done' is met
  def processUntil[T](data: List[T])(done: List[T] => Boolean)(fx: List[T] => List[T]):List[T]={
    if (!done(data)){
      processUntil(fx(data))(done)(fx)
    }
    else{
      data
    }
  }


  //combine list of code tree leaves into an actual tree
  def combineCodeTrees(xs: List[CodeTree]): List[CodeTree]={

    if (xs.length == 1)
      xs
    else{

      val  f = Fork(xs.head,xs.tail.head)//combine two least weighted nodes
      val (less, greater) = xs.tail.tail.span(_.weight < f.weight) // partition remaining leaves around the new fork's weight

      //combine results
      less ::: List(f) ::: greater
    }
  }


  //generate a code tree from a source of characters
  def makeCodeTreeFromText(text: Iterable[Char]): CodeTree = makeCodeTreeFromFreqs(countFrequency(text))

  def makeCodeTreeFromFreqs(frequencyMap: Map[Char,Int]): CodeTree ={


    val charsByAscendingFreq = frequencyMap.toList.sortBy({case(c,f) => f}) //sort by ascending frequency
    val leaves = charsByAscendingFreq.map{case(c,f) => Leaf(c,f)} //build a list of leaves

    processUntil[CodeTree](leaves)(_.length == 1)(combineCodeTrees).head //reduce the list into an actual tree structure
  }


  def countFrequency(text: Iterable[Char]): Map[Char,Int]= text.foldLeft(Map.empty[Char,Int])((map,char) => map + (char -> (map.getOrElse(char,0) + 1)))

  //descends the code tree, chomping off bits until we hit a leaf
  //returns the character we produced and the remainings bits
  def decode(tree: CodeTree, bits: List[Int]): List[Char] = {
    def descend(tree: CodeTree, bits: List[Int]): (Char, List[Int]) = tree match {
      case Fork(left, right) => descend(if (bits.head == 0) left else right, bits.tail)
      case Leaf(char, weight) => (char, bits) //we found a leaf node and we're done descending
    }
    def loop(bits: List[Int], acc: List[Char] = Nil): List[Char] = {
      if (bits.isEmpty)
        acc
      else {
        val (c, remainingBits) = descend(tree, bits)
        loop(remainingBits, acc ::: List(c))
      }
    }

    loop(bits)
  }

  def encode(code: CodeTree, text: Iterable[Char], acc: List[Int] = Nil): List[Int]={

    def getCode(char: Char, root: CodeTree, acc: List[Int] = Nil): List[Int] = root match{
      case Leaf(_,_) => acc
      case Fork(left,right) => {
        if (left.chars.contains(char))
          getCode(char,left, acc ::: List(0))
        else
          getCode(char,right, acc ::: List(1))
      }
    }



    if (text.isEmpty)
      acc
    else{
      val bits = getCode(text.head,code)
      encode(code,text.tail, acc ::: bits)
    }
  }

}
