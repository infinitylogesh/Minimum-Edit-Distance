package com.logesh.feelers

import scala.collection.mutable.ListBuffer

class EditDistance(private val target:String,private val source:String) {

  private var transcript = ListBuffer[Char]()

  // Returns the transcript of the difference between the two Strings. M -> Match , S -> Substitute , D -> Deletion, I -> Insert,

  def getTranscript():ListBuffer[Char] = {
    if(transcript.length>0)
      return this.transcript;
    else{
      var dist = this.minEditDistance();
      return this.transcript;
    }
  }

  // Minimum edit distance implemented using dynamic programing.

  def minEditDistance():Int = {
    val m = target.length;
    val n = source.length;
    var distance = (0 to m).map(y=>(0 to n).map(x=>0).toArray).toArray;

    for( x <- (0 to m))
      distance(x)(0) = x;

    for(y <- (0 to n))
      distance(0)(y) = y;

    for(i <- (1 to m);j <- (1 to n)){
      distance(i)(j) = Array((distance(i-1)(j)+1),(distance(i)(j-1)+1),(distance(i-1)(j-1)+substutionCost(target(i-1),source(j-1)))).min;
    }

    getTracePoints(m,n,distance);
    distance(m)(n);  // last cell of the table holds the edit distance and the same is returned.
  }

  // Substitution cost of a character is determined in this function.

  def substutionCost(target: Char, source: Char): Int = {
    if (target == source) 0 else 2
  }

  // Backtrace is implemented and all the edits are appended to transcript List buffer.

  def getTracePoints(m:Int,n:Int,distance: Array[Array[Int]]):Unit = {
      if((m-1 >= 0) && (n-1 >= 0))  // to avoid index out of bound error.
            if(target(m-1)==source(n-1)){  // incase of the target and source characters are same.
              transcript.append('M');
              getTracePoints(m-1,n-1,distance);
            }
            else{ // In case the characters are different . Minimum value and the action corresponding is taken into the List buffer.
              val optionList = List((distance(m-1)(n),(m-1,n),'D'),(distance(m)(n-1),(m,n-1),'I'),(distance(m-1)(n-1),(m-1,n-1),'S'));
              val minTuple = optionList.minBy(_._1);
              transcript.append(minTuple._3);
              getTracePoints(minTuple._2._1,minTuple._2._2,distance);
            }
      else
        if(m==1 && n==0)
          transcript.append('D');
        else if(m==0 && n==1)
          transcript.append('I');
  }

}

// Factory object.

object EditDistance{
  def apply(target:String,source:String):EditDistance = {
    var editDistance = new EditDistance(target,source);
    return editDistance;
  }

}