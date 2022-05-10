object HelloWorld {
  def isNumberInRow(board:Array[Array[Int]],number:Int,row:Int):Boolean =
  {
    for(i<-0 until 9){
      if(board(row)(i) == number){
        return true
      }
    }
    return false
  }
  
  def isNumberInColumn(board:Array[Array[Int]],number:Int,column:Int):Boolean =
  {
    for(j<-0 until 9){
      if(board(j)(column) == number){
        return true
      }
    }
    return false
  }
  
  def isNumberInBox(board:Array[Array[Int]],number:Int,row:Int,column:Int):Boolean =
  {
    var localBoxRow = row - row % 3
    var localBoxColumn =column - column % 3
    for(m <- localBoxRow until localBoxRow + 3){
      for(n <- localBoxColumn until localBoxColumn + 3){
        if (board(m)(n) == number){
          return true
        }
      }
    }
    return false
  }
  
  def isValidPlacement(board:Array[Array[Int]],number:Int,row:Int,column:Int):Boolean =
  {
    if(isNumberInRow(board, number, row) == false &&
       isNumberInColumn(board, number, column) == false &&
       isNumberInBox(board, number, row, column) == false){
         return true
       }
    return false   
  }
  
  def solveBoard(board:Array[Array[Int]]): Boolean =
  {
    for(r <- 0 until 9){
      for(c <- 0 until 9){
        if(board(r)(c) == 0){
          for(numberToTry <- 1 to 9){
            if(isValidPlacement(board, numberToTry, r, c) == true){
              board(r)(c) = numberToTry;
              
              if (solveBoard(board)){
                return true;
              }
              else{
                board(r)(c) = 0;
              }
            }
          }
          return false
        }
      }
    }
    return true
  }
  
  def printBoard(board:Array[Array[Int]]): Unit =
  {
    for(a <- 0 until 9){
      if ( a % 3 == 0 && a != 0){
        println("-----------");
      }
      for(b <- 0 until 9){
        if( b % 3 == 0 && b != 0){
          print("|");
        }
        print(board(a)(b))
      }
      println()
    }
  }
    
	def main(args: Array[String]): Unit = {
	var board = Array( Array(7,0,2,0,5,0,6,0,0),
	                   Array(0,0,0,0,0,3,0,0,0),
	                   Array(1,0,0,0,0,9,5,0,0),
	                   Array(8,0,0,0,0,0,0,9,0),
	                   Array(0,4,3,0,0,0,7,5,0),
	                   Array(0,9,0,0,0,0,0,0,8),
	                   Array(0,0,9,7,0,0,0,0,5),
	                   Array(0,0,0,2,0,0,0,0,0),
	                   Array(0,0,7,0,4,0,2,0,3));
	                   
	printBoard(board);
	println();
	
	if(solveBoard(board) == true)
  {
      println("Solved Successfully!");
  }
  else
  {
      println("Unsolvable board :(");
  }
	                   

	 printBoard(board);
	}
}
