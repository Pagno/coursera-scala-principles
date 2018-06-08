package recfun

object Main {
  def main(args: Array[String]) {
	  
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
  	}
	println("countChange")
	countChange(4,List(1,2))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
    	if (c == r || c == 0) 1
    	else pascal(c - 1, r - 1) + pascal(c, r - 1)
	}
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
		val x = chars.foldLeft(0: Int)({
			(number: Int, current: Char)=>{
				number match{
					case -1 => number
					case _ => {
						current match {
							case '(' =>  number+1
							case ')' => number-1
							case _ => number
						}
					}
				}
				
			}
		})
		x==0
	}

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
		def ways(change: List[Int], size: Int, capacity: Int): Int = {
			if(capacity == 0){
				return 1
			}else if((capacity < 0) || (size <= 0)){
				return 0
			}
			ways(change, size-1, capacity) + ways(change, size, capacity - change(size - 1))
		}
		ways(coins, coins.length, money)
	}
}
