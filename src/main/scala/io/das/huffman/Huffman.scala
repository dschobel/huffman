package io.das.huffman


object Huffman{
  def makeEncoderFromCorpus(corpus: Iterable[String]): Huffman= ???
  def makeEncoderFromMap(map: Map[Char,Int]): Huffman = ???


  def processUntil[T](data: List[T])(done: List[T] => Boolean)(fx: List[T] => List[T]):List[T]={
    if (!done(data)){
      processUntil(fx(data))(done)(fx)
    }
    else{
      data
    }
  }


  //take the two highest weight leaves and combine them into a new fork
  def processCodeTrees(xs: List[CodeTree.CodeTree]): List[CodeTree.CodeTree]={

    if (xs.length == 1)
      xs
    else{
      CodeTree.Fork(xs.head,xs.tail.head) :: xs.tail.tail
    }
  }


  def makeCodeTreeFromFreqs(frequencyMap: Map[Char,Int]): CodeTree.CodeTree ={

    val charsByDescendingFreq = frequencyMap.toList.sortWith{case((c1,i1),(c2,i2)) => i1 > i2}
    val leaves = charsByDescendingFreq.map{case(c,i) => CodeTree.Leaf(c,i)}

    processUntil[CodeTree.CodeTree](leaves)(_.length == 1)(processCodeTrees).head
  }


  def countFrequency(text: Iterable[Char]): Map[Char,Int]={
    text.foldLeft(Map.empty[Char,Int])((map,char) => map + (char -> (map.getOrElse(char,0) + 1)))
  }
}

class Huffman(val corpus: Iterable[String]) {
  type Bit = Int
  def encode(text: Iterable[Char]): Stream[Bit] = ???
  def decode(coded: Iterable[Bit]): Stream[Char] = ???
}
