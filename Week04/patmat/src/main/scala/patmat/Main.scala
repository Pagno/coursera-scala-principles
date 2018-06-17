package patmat

import patmat.Huffman._

object Main {
  def main(args: Array[String]) {
	val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
	val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	  
	def iterate(tree: CodeTree, bits: List[Bit],charsList: List[Char]): List[Char] = tree match {
			case Leaf(char: Char, weight: Int) => {
				println("Leaf " + charsList + " - " + bits)
				if(bits.length==0){
					return charsList:::List[Char](char)
				}else{
					return iterate(t1, bits, charsList:::List[Char](char))
				}
			}
			case Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) => {
				println("Fork " + charsList + " - " + bits)
				if(bits.head == 0)
					return iterate(left,bits.tail,charsList)
				else
					return iterate(right,bits.tail,charsList)
				
			}
			case _ => List[Char]()
		}
	//println(encode(t1)("ab".toList))
	val a = "aaabbc".toList
	println(createCodeTree(a))
	
  }
}
